# -*- coding: UTF-8 -*-
import sys
import easyvvuq as uq
from templates.cpo_encoder import CPOEncoder


# From easyvvuq/tools
def encode(params):
    db_type = params[1]
    db_location = params[2]
    write_to_db = params[3]
    campaign_name = params[4]
    app_name = params[5]
    run_id_list = params[6].split(',')

    if write_to_db == 'TRUE':
        write_to_db_bool = True
    elif write_to_db == 'FALSE':
        write_to_db_bool = False
    else:
        sys.exit("write_to_db arg must be TRUE or FALSE")

    worker = uq.Worker(
        db_type=db_type,
        db_location=db_location,
        campaign_name=campaign_name,
        app_name=app_name,
        write_to_db=write_to_db_bool)

    worker.encode_runs(run_id_list)

if __name__ == "__main__":

    if len(sys.argv) != 7:
        sys.exit(
            (f"Usage: python3 external_encoder.py db_type db_location "
             "write_to_db{'TRUE' or 'FALSE'} campaign_name app_name comma_separated_run_id_list")
        )

    encode(sys.argv)
