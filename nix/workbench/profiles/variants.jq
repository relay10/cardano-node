## Profile variants are defined as a cartesian product of
## variations of genesis/generator/node axes.

def genesis_profile_variants:
    { scenario: "fixed-loaded"
    , composition:
      { n_singular_hosts:               2
      , n_dense_hosts:                  0
      }
    , genesis:
      { utxo:                           6000
      , delegators:                     1300
      , max_block_size:                 80000
      , epoch_length:                   600
      , parameter_k:                    3
      }
    , node:
      { shutdown_on_slot_synced: 10
      }
    , generator: { tps: 15 }
    , analysis:
      { type:                           "standard"
      }
    } as $quick_base
  |
    { scenario: "fixed-loaded"
    , composition:
      { n_singular_hosts:               2
      , n_dense_hosts:                  0
      }
    , genesis:
      { utxo:                           6000000
      , delegators:                     1300000
      , max_block_size:                 80000
      , epoch_length:                   600
      , parameter_k:                    3
      }
    , node:
      { shutdown_on_slot_synced: 2400
      }
    , generator: { tps: 15 }
    } as $forge_stress_base
  |
    { genesis:
      { alonzo:
        { maxTxExUnits:
          { exUnitsMem:                 12500000
          }
        }
      }
    , generator:
      { inputs_per_tx:                  1
      , outputs_per_tx:                 1
      , plutusMode:                     true
      , plutusAutoMode:                 true
      }
    , analysis:
      { filters:                        ["base", "size-small"]
      }
    } as $plutus_base
  |
    { scenario: "chainsync"
    , preset: "mainnet"
    , composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_chaindb_server:            true
      , with_observer:                  true
      }
    , analysis:
      { type:                           "performance"
      }
    } as $chainsync_base
  |
    { node:
      { tracing_backend:                "iohk-monitoring"
      }
    } as $old_tracing
  |
    { node:
      { mainnet_chaindb_upto_chunk:     10
      , shutdown_on_slot_synced:
        { observer:                     237599
        }
      }
    } as $chaindb_early_byron
  |
    { node:
      { mainnet_chaindb_upto_chunk:     1800
      , shutdown_on_slot_synced:
        { observer:                     47173650 # 37173650
        }
      }
    } as $chaindb_early_alonzo
  |

  ## Baseline:
  [ { genesis: { utxo: 4000000, delegators: 1000000 } }

  ## Baseline, tweaked for fast local repro:
  , { genesis: { utxo: 4000000, delegators: 1000000
               , slot_duration: 0.2 }
    , composition: { with_observer: false }}

  ## Size-varied derivatives of baseline:
  , { genesis: { utxo: 4000000, delegators: 1000000 } }
  , { genesis: { utxo: 5000000, delegators: 1250000 } }
  , { genesis: { utxo: 6000000, delegators: 1500000 } }
  , { genesis: { utxo: 4000000, delegators: 1000000, dense_pool_density:  2 } }
  , { genesis: { utxo: 4000000, delegators: 1000000, dense_pool_density:  3 } }
  , { genesis: { utxo: 4000000, delegators: 1000000, dense_pool_density: 10 } }

  ## TPS-varied derivatives of baseline:
  , { genesis: { utxo: 4000000, delegators: 1000000 }
    , generator: { tps: 5 } }
  , { genesis: { utxo: 4000000, delegators: 1000000 }
    , generator: { tps: 10 } }

  ## Calibration:
  , { genesis: { utxo: 2000000, delegators: 1000000, max_block_size:  128000 }
    , generator: { tps:  16 } }
  , { genesis: { utxo: 2000000, delegators: 1000000, max_block_size:  256000 }
    , generator: { tps:  32 } }
  , { genesis: { utxo: 2000000, delegators:  500000, max_block_size:  512000 }
    , generator: { tps:  64 } }
  , { genesis: { utxo: 2000000, delegators:  500000, max_block_size: 1024000 }
    , generator: { tps: 128 } }
  , { genesis: { utxo: 2000000, delegators:  500000, max_block_size: 2048000 }
    , generator: { tps: 256 } }

  ## Fixed
  , { name: "fixed"
    , scenario: "fixed"
    , genesis: { utxo: 4000000, delegators: 1000000 }
    , node:
      { shutdown_on_slot_synced: 150
      }
    }
  , { name: "smoke"
    , scenario: "fixed-loaded"
    , node:
      { shutdown_on_slot_synced: 60
      }
    , generator: { tps: 10 }
    }
  , { scenario: "fixed-loaded"
    , genesis: { utxo: 1000000, delegators: 1000000 }
    , node:
      { shutdown_on_slot_synced: 600
      }
    , generator: { tps: 10 }
    }

  , $forge_stress_base *
    { name: "forge-stress"
    }

  , $forge_stress_base * $old_tracing *
    { name: "forge-stress-oldtracing"
    }

  , $forge_stress_base *
    $plutus_base *
    { name: "forge-stress-plutus"
    , composition:
      { n_singular_hosts:               1
      }
    , generator:
      { tx_count:                       800
      }
    }

  , $quick_base *
    { name: "quick"
    }

  , $quick_base * $old_tracing *
    { name: "quick-oldtracing"
    }

  , $chainsync_base * $chaindb_early_byron *
    { name: "chainsync-early-byron"
    }
  , $chainsync_base * $chaindb_early_byron * $old_tracing *
    { name: "chainsync-early-byron-oldtracing"
    }

  , $chainsync_base * $chaindb_early_alonzo *
    { name: "chainsync-early-alonzo"
    }
  , $chainsync_base * $chaindb_early_alonzo * $old_tracing *
    { name: "chainsync-early-alonzo-oldtracing"
    }
  ];

def generator_profile_variants:
  [ { generator: {} }
  ];

def node_profile_variants:
  [ { node: {} }
  ];

def       all_profile_variants:
  [   genesis_profile_variants
  , generator_profile_variants
  ,      node_profile_variants
  ]
  | [combinations]
  | map (reduce .[] as $item ({}; . * $item));
