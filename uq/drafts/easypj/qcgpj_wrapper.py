# -*- coding: UTF-8 -*-
import os
import easyvvuq as uq
from tempfile import mkdtemp
from qcg.appscheduler.api.job import Jobs
from qcg.appscheduler.api.manager import LocalManager

# A lightweight wrapper enabling the encoding and the execution
# using the QCG Pilot Job mechanism.


# TODO add args for interctive mode, or use class
def run(campaign, exec_path, ):
    # Current directory
    cwd = os.getcwd()

    print('>> QCG - Pilot Job: START')
    # Set QCG-PJ temp directory
    qcgpj_tempdir = mkdtemp(None, ".qcgpj-", campaign.campaign_dir)

    # Switch on debugging of QCGPJ API (client part)
    client_conf = {'log_file': qcgpj_tempdir + '/api.log', 'log_level': 'DEBUG'}

    # create local LocalManager (service part)
    m = LocalManager(['--log', 'debug', '--wd', qcgpj_tempdir], client_conf)
    print(">> Available resources:\n%s\n" % str(m.resources()))

    # Execute encode -> execute for each run using QCG-PJ
    print(">> Starting submission of tasks to QCG Pilot Job Manager")

    encoder_path = os.path.realpath(os.path.expanduser("easypj/easyvvuq_encode"))
    execute_path = os.path.realpath(os.path.expanduser("easypj/easyvvuq_execute"))
    app_path = os.path.realpath(os.path.expanduser("easypj/easyvvuq_app"))

    for run in campaign.list_runs():

        key = run[0]
        run_dir = run[1]['run_dir']

        enc_args = [
            campaign.db_type,
            campaign.db_location,
            'FALSE',
            campaign.campaign_name,
            campaign.campaign_name,
            key
        ]

        exec_args = [
            run_dir,
            app_path,
            exec_path
        ]

        encode_task = {
            "name": 'encode_' + key,
            "execution": {
                "exec": encoder_path,
                "args": enc_args,
                "wd": cwd,
                "stdout": qcgpj_tempdir + '/encode_' + key + '.stdout',
                "stderr": qcgpj_tempdir + '/encode_' + key + '.stderr'
            },
            "resources": {
                "numCores": {
                    "exact": 1
                }
            }
        }

        execute_task = {
            "name": 'execute_' + key,
            "execution": {
                "exec": execute_path,
                "args": exec_args,
                "wd": cwd,
                "stdout": qcgpj_tempdir + '/execute_' + key + '.stdout',
                "stderr": qcgpj_tempdir + '/execute_' + key + '.stderr'
            },
            "resources": {
                "numCores": {
                    "exact": 1
                }
            },
            "dependencies": {
                "after": ["encode_" + key]
            }
        }

        m.submit(Jobs().addStd(encode_task))
        m.submit(Jobs().addStd(execute_task))

    # Wait for completion of all PJ tasks and terminate the PJ manager
    print(">> Wait for completion of all PJ tasks")
    m.wait4all()
    m.finish()
    m.stopManager()
    m.cleanup()

    print(">> Syncing state of campaign after execution of PJ")
    def update_status(run_id, run_data):
        campaign.campaign_db.set_run_statuses([run_id], uq.constants.Status.ENCODED)

    campaign.call_for_each_run(update_status, status=uq.constants.Status.NEW)

    print('>> QCG - Pilot Job: END')
