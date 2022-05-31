from typing import Tuple, Dict, Any

import easyvvuq as uq

#from qcg.pilotjob.executor_api.qcgpj_executor import QCGPJExecutor
from qcg.pilotjob.executor_api.templates.qcgpj_template import QCGPJTemplate

class EasyVVUQParallelTemplateWithEnv(QCGPJTemplate):

    @staticmethod
    def template() -> Tuple[str, Dict[str, Any]]:
        template = """
            {
                'name': '${name}',
                'execution': {
                    'exec': '${exec}',
                    'args': ${args},
                    'stdout': '${stdout}',
                    'stderr': '${stderr}',
                    'venv': '${venv}',
                    'model': '${model}',
                    'model_opts': ${model_opts},
                    'env': {
                         'I_MPI_HYDRA_BOOTSTRAP_EXEC_EXTRA_ARGS': '--exclusive'
                    }
                },
                'resources': {
                    'numCores': {
                        'exact': ${numCores}
                    },
                    'numNodes': {
                        'exact': ${numNodes}
                    }
                }
            }
             """

        defaults = {
            'args': [],
            'stdout': 'stdout',
            'stderr': 'stderr',
            'venv': '',
            'model': 'default',
            'model_opts': {},
            'numCores': 1,
            'numNodes': 1
        }

        return template, defaults

