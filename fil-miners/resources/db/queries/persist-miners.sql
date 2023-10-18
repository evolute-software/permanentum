
INSERT INTO
    filrep_id,

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
VALUES (?)
ON CONFLICT (filrep_id) DO UPDATE

