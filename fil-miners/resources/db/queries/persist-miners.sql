
INSERT INTO filrep_miners(
    address,

    price,
    region,
    iso2_code,
    rank,

    status,
    reachable,
    score,

    free_space,
    min_piece_size,
    max_piece_size,

    deals_total,
    deals_pristine,
    deals_slashed,
    deals_terminated,
    deals_fault_terminated
)
VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
ON CONFLICT (address) DO
UPDATE SET
    price                   =  EXCLUDED.price,
    region                  =  EXCLUDED.region,
    iso2_code               =  EXCLUDED.iso2_code,
    rank                    =  EXCLUDED.rank,

    status                  =  EXCLUDED.status,
    reachable               =  EXCLUDED.reachable,
    score                   =  EXCLUDED.score,

    free_space              =  EXCLUDED.free_space,
    min_piece_size          =  EXCLUDED.min_piece_size,
    max_piece_size          =  EXCLUDED.max_piece_size,

    deals_total             =  EXCLUDED.deals_total,
    deals_pristine          =  EXCLUDED.deals_pristine,
    deals_slashed           =  EXCLUDED.deals_slashed,
    deals_terminated        =  EXCLUDED.deals_terminated,
    deals_fault_terminated  =  EXCLUDED.deals_fault_terminated
;

