usage_chaindb() {
     usage "chaindb" "Manage ChainDBs" <<EOF
  shared args:
    immutable-until-chunk
                     Produce a ChainDB derivative trimmed up to specified chunk#
    snapshot-at-slot Produce a ChainDB derivative with snapshot at given slot#
EOF
}

chaindb() {
local source= geneses_json={}
while test $# -gt 0
do case "$1" in
       --source )       source=$2; shift;;
       --geneses-json ) geneses_json=$2; shift;;
       * ) break;; esac; shift; done

op=${1:?$(usage_chaindb)}; shift

case "$op" in

immutable-until-chunk )
    local usage="USAGE: wb chaindb --source IMMUTABLEDB-SRCDIR $op IMMUTABLEDB-OUTDIR FINAL-CHUNKNO"
    local out=${1:?$usage}; shift
    local final_chunkno=${1:?$usage}; shift

    (
    mkdir -p $out/immutable
    cd $out
    cp ${source}/protocolMagicId protocolMagicId
    for epoch in {00000..$((final_chunkno - 1))}; do
      ln -s ${source}/immutable/${epoch}.{chunk,primary,secondary} immutable
    done
    cp ${source}/immutable/$(printf "%05d" $final_chunkno).{chunk,primary,secondary} immutable

    ## Existence of 'clean' is necessary to avoid ImmutableDB revalidation:
    touch clean
    );;

snapshot-at-slot )
    local usage="USAGE: wb chaindb --source IMMUTABLEDB-SRCDIR $op SNAPSHOT-OUTDIR SLOTNO"
    local out=${1:?$usage}; shift
    local slotno=${1:?$usage}; shift
    local args=()

    cp -r ${source}  $out
    chmod +w -R      $out

    args=( --genesis "$(jq .shelley <<<$geneses)"
         )
    local shelley_genesis_hash=$(cardano-cli genesis hash "${args[@]}" 2>/dev/null)
    test -n "$shelley_genesis_hash" ||
        fail "Invalid Shelley genesis: $(jq .shelley <<<$geneses)"

    ## Actually produce the snapshot:
    args=( --configByron     "$(jq .byron   <<<$geneses)"
           --configShelley   "$(jq .shelley <<<$geneses)"
           --configAlonzo    "$(jq .alonzo <<<$geneses)"
           --nonce           $shelley_genesis_hash
           --store-ledger    $slotno
         )
    db-analyser --db $out cardano "${args[@]}"

    ls -ltrh         $out/ledger

    mv               $out/ledger/${toString snapshotSlot}_db-analyser $out/temp
    rm               $out/ledger/*
    mv $out/temp     $out/ledger/${toString snapshotSlot}

    local last_chunk=$(ls $source | tail -n1 | cut -d. -f1)

    args=( --argjson snapshotSlot       $slotno
           --arg     finalChunkNo       $last_chunk
           --arg     shelleyGenesisHash $shelley_genesis_hash
         )
    jq '{ snapshotSlot:                 $snapshotSlot
        , finalChunkNo:                 $finalChunkNo
        , shelleyGenesisHash:           $shelleyGenesisHash
      # , snapshottingConsensusNodeRev: $snapshottingConsensusNodeRev
        }
       ' "''${args[@]}" > $out/snapshot-info.json
    ;;

* ) usage_chaindb;; esac
}
