usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    standard RUN-NAME..   Standard batch of analyses: block-propagation, and
                            machine-timeline

    call RUN-NAME OPS..   Execute 'locli' "uops" on the specified run

    Options of 'analyse' command:

       --filters F,F,F..  Comma-separated list of named chain filters:  see bench/chain-filters
                            Note: filter names have no .json suffix
       --dump-logobjects  Dump the intermediate data: lifted log objects
EOF
}

analyse() {
local filters=() aws=
local dump_logobjects= dump_machviews= dump_chain_raw= dump_chain= dump_slots_raw= dump_slots=
while test $# -gt 0
do case "$1" in
       --dump-logobjects | -lo )  dump_logobjects='true';;
       --dump-machviews  | -mw )  dump_machviews='true';;
       --dump-chain-raw  | -cr )  dump_chain_raw='true';;
       --dump-chain      | -c )   dump_chain='true';;
       --filters )                analysis_set_filters "base,$2"; shift;;
       --no-filters )             analysis_set_filters "";;
       * ) break;; esac; shift; done

if curl --connect-timeout 0.5 http://169.254.169.254/latest/meta-data >/dev/null 2>&1
then aws='true'; fi

## Work around the odd parallelism bug killing performance on AWS:
if test -n "$aws"
then locli_rts_args=(+RTS -N1 -A128M -RTS)
     echo "{ \"aws\": true }"
else locli_rts_args=()
     echo "{ \"aws\": false }"
fi

local op=${1:-standard}; if test $# != 0; then shift; fi

case "$op" in
    # 'read-mach-views' "${logs[@]/#/--log }"
    standard | std )
        local runs=($*); if test $# = 0; then runs=(current); fi

        for r in ${runs[*]}
        do analyse prepare $r

           local name=${1:-current}; if test $# != 0; then shift; fi
           local dir=$(run get "$name")
           local adir=$dir/analysis
           test -n "$dir" -a -d "$adir" || fail "malformed run: $name"

           if test -z "${filters[*]}"
           then local filter_names=$(jq '.analysis.filters
                                        | join(",")
                                        ' "$dir"/profile.json --raw-output)
                analysis_set_filters "$filter_names"
           fi

           local logs=("$adir"/logs-*.flt.json)
           local args=(
               'meta-genesis'         --run-metafile    "$dir"/meta.json
                                      --shelley-genesis "$dir"/genesis-shelley.json

               'unlog' --host-from-log-filename ${logs[@]/#/--log }
               $(if test -n "$dump_logobjects"; then echo \
                 'dump-logobjects'; fi)

               'build-mach-views'
               $(if test -n "$dump_machviews"; then echo \
                 'dump-mach-views'; fi)

               'build-chain'
               $(if test -n "$dump_chain_raw"; then echo \
                 'dump-chain-raw'     --chain           "$adir"/chain-raw.json; fi)

               'timeline-chain-raw'   --timeline        "$adir"/chain-raw.txt

               'filter-chain' "${filters[@]}"
               $(if test -n "$dump_chain"; then echo \
                 'dump-chain'         --chain           "$adir"/chain.json; fi)

               'timeline-chain'       --timeline        "$adir"/chain.txt

               'collect-slots'
               $(if test -n "$dump_slots_raw"; then echo \
                 'dump-slots-raw'; fi)

               'filter-slots' "${filters[@]}"
               $(if test -n "$dump_slots"; then echo \
                 'dump-slots'; fi)

               'timeline-slots'

               'propagation'
               'dump-propagation'     --analysis        "$adir"/blockprop.json
               'report-prop-forger'   --report          "$adir"/blockprop-forger.txt
               'report-prop-peers'    --report          "$adir"/blockprop-peers.txt
               'report-prop-endtoend' --report          "$adir"/blockprop-endtoend.txt
               'report-prop-full'     --report          "$adir"/blockprop-full.txt

               'perfanalysis'
               'dump-perfanalysis'
               'report-perf-full'
               'report-perf-brief'
           )
           time locli "${locli_rts_args[@]}" "${args[@]}"
        done
        ;;

    call )
        local usage="USAGE: wb analyse $op RUN-NAME OPS.."

        local name=${1:?$usage}; shift
        local dir=$(run get "$name")
        local adir=$dir/analysis
        test -n "$dir" -a -d "$adir" || fail "malformed run: $name"

        local logfiles=("$adir"/logs-*.flt.json)
        local logs=(       'unlog'
                           ${logfiles[*]/#/--log })
        local run=(       'meta-genesis'
                           --run-metafile    "$dir"/meta.json
                           --shelley-genesis "$dir"/genesis-shelley.json)
        local chain_raw=( 'dump-chain-raw'
                          --chain     "$adir"/chain-raw.json )
        local chain=(     'dump-chain-raw'
                          --chain     "$adir"/chain.json )
        local flt_chain=( 'filter-chain'
                          "${filters[@]}")
        local ops0=("$@")
        local ops1=(${ops0[*]/#auto-logs/${logs[*]}})
        local ops2=(${ops1[*]/#auto-run/${run[*]}})
        local ops3=(${ops2[*]/#auto-dump-chain-raw/${chain_raw[*]}})
        local ops4=(${ops3[*]/#auto-filter-chain/${flt_chain[*]}})
        local ops5=(${ops4[*]/#auto-dump-chain/${chain[*]}})
        local ops_final=(${ops5[*]})

        echo locli "${locli_rts_args[@]}" "${ops_final[@]}"
        time locli "${locli_rts_args[@]}" "${ops_final[@]}"
        ;;

    prepare | prep )
        local usage="USAGE: wb analyse $op [RUN-NAME=current].."

        local name=${1:-current}; if test $# != 0; then shift; fi
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        local adir=$dir/analysis
        mkdir -p "$adir"

        ## 0. ask locli what it cares about
        local keyfile="$adir"/substring-keys
        case $(jq '.node.tracing_backend // "trace-dispatcher"' --raw-output $dir/profile.json) in
             trace-dispatcher ) locli 'list-logobject-keys'        --keys        "$keyfile";;
             iohk-monitoring  ) locli 'list-logobject-keys-legacy' --keys-legacy "$keyfile";;
        esac

        ## 1. unless already done, filter logs according to locli's requirements
        local logdirs=($(ls -d "$dir"/node-*/ 2>/dev/null))
        local logfiles=($(ls "$adir"/logs-node-*.flt.json 2>/dev/null))
        local prefilter=$(test -z "${logfiles[*]}" && echo 'true' || echo 'false')
        echo "{ \"prefilter\": $prefilter }"
        if test x$prefilter != xtrue
        then return; fi

        local jq_args=(
            --sort-keys
            --compact-output
            'delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])'
        )
        for d in "${logdirs[@]}"
        do throttle_shell_job_spawns
           local logfiles="$(ls "$d"/stdout* 2>/dev/null | tac) $(ls "$d"/node-*.json 2>/dev/null)"
           if test -z "$logfiles"
           then msg "no logs in $d, skipping.."; fi
           local output="$adir"/logs-$(basename "$d").flt.json
           grep -hFf "$keyfile" $logfiles > "$output" &
        done

        wait;;

    * ) usage_analyse;; esac
}

num_jobs="\j"
num_threads=$({ grep processor /proc/cpuinfo 2>/dev/null || echo -e '\n\n\n';
              } | wc -l)

throttle_shell_job_spawns() {
    sleep 0.5s
    while ((${num_jobs@P} >= num_threads - 4))
    do wait -n; sleep 0.$(((RANDOM % 5) + 1))s; done
}

analysis_set_filters() {
    local filter_names=($(echo $1 | sed 's_,_ _g'))
    local filter_paths=(${filter_names[*]/#/"$global_basedir/chain-filters/"})
    local filter_files=(${filter_paths[*]/%/.json})

    for f in ${filter_files[*]}
    do test -f "$f" ||
            fail "no such filter: $f"; done

    filters+=(${filter_files[*]/#/--filter })
}

analysis_classify_traces() {
    local name=${1:-current}; if test $# != 0; then shift; fi
    local dir=$(run get "$name")

    progress "analysis" "enumerating namespace from logs of node-0.."
    local types=($(grep -h '^{' $dir/node-0/stdout* | jq --raw-output '.ns' 2>/dev/null | tr -d ']["' | sort -u))

    progress_ne "analysis" "collecting occurence stats: "
    for node in $dir/node-*/
    do for type in ${types[*]}
       do echo $(grep -hF "\"$type\"" $node/stdout* | wc -l) $type
       done |
           sort -nr > $node/log-namespace-occurence-stats.txt
       echo -n ' '$(basename $node) >&2
    done
    echo >&2
}
