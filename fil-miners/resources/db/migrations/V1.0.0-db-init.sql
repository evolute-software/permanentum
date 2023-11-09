-- Base DB for a permanentum instance

-- TODO: Move more varchars into bounded domains
-- TODO: Reduce nullables

-- filrep regions
CREATE TYPE filrep_region AS ENUM ('Asia', 'Europe', 'Africa', 'Oceania', 'NorthAmerica', 'CentralAmerica', 'SouthAmerica', 'Unknown');

-- filrep_miners table
CREATE TABLE filrep_miners (
   address char(12) NOT NULL,

   price numeric(27),
   region filrep_region NOT NULL,
   iso2_code char(2),
   rank numeric(10) NOT NULL,

   status boolean NOT NULL,
   reachable boolean NOT NULL,
   score numeric(2), -- 00 to 99

   free_space numeric(21), -- 0 bytes to 999Eib
   min_piece_size numeric(16), -- 0bytes to 9Pib
   max_piece_size numeric(16), -- should be enough

   deals_total numeric(10) NOT NULL,
   deals_pristine numeric(10) NOT NULL,
   deals_slashed numeric(10) NOT NULL,
   deals_terminated numeric(10) NOT NULL,
   deals_fault_terminated numeric(10) NOT NULL, -- Undocumented

   created_at numeric(18) NOT NULL,
   updated_at numeric(18) NOT NULL,

   CONSTRAINT filrep_miner_pkey PRIMARY KEY (address)
);

CREATE FUNCTION micro_unix_ts()
RETURNS numeric(18)
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN ((EXTRACT(epoch FROM (current_timestamp AT TIME ZONE 'UTC')) * 1000000) :: numeric(18));
END;
$$;


CREATE FUNCTION insert_ctime_mtime()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
    NEW.created_at = micro_unix_ts();
    NEW.updated_at = NEW.created_at;
    RETURN NEW;
END;
$$;

CREATE FUNCTION update_mtime()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
    NEW.updated_at = micro_unix_ts();
    RETURN NEW;
END;
$$;

CREATE TRIGGER update_miner_mtime BEFORE
UPDATE ON filrep_miners FOR EACH ROW
EXECUTE PROCEDURE update_mtime();

CREATE TRIGGER insert_miner_timings BEFORE
INSERT ON filrep_miners FOR EACH ROW
EXECUTE PROCEDURE insert_ctime_mtime();

