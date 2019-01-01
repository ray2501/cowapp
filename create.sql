create table if not exists Notes (id uuid DEFAULT md5(random()::text || clock_timestamp()::text)::uuid, 
title varchar(255), body text, created timestamp, PRIMARY KEY (id));
