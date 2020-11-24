from toy_surrogate import *
from toy_surrogate import ExtCodeHelper
from da_utils import read_sim_csv
from joblib import load

from extcodehelper import ExtCodeHelper

#steps: 
# 1) read data in gradTe, Teflux from file +
# 2) get histogram on Teflux 
# 3) get mean Teflux -> target +
# 4) get opt_param: func(param)==target 
# -> first stop

def target_distribution(source, target_names=["ti_flux"]):
    if isinstance(source, str):
        # dataframe with sim results, after UQP1
        #simdata, _, _ = read_sim_csv(source)
        simdata = pd.read_table(source, delimiter='  *', engine='python')

    target_distributions = {}

    # dictionary of densities
    for target_name in target_names:
        target_distributions[target_name] = simdata[[target_name]] # TODO: denisity in a separate data structure 
    return target_distributions

def target_expectation(target_distribution):

    return target_distribution.mean()

def qss_input(function, input_domain, input_distribution, output_distribution, output_predicted, input_names={"ti_value", "ti_ddrho"}, output_names={"ti_flux"}):
    """
    Returns candidate of input values for which output is in quasy steady state
    QSS candidate is mean value of output (in last window)
    """

    # mean of output Ey
    targ = target_expectation(output_distribution).to_numpy()[0]

    # find all x: Ey=F(x)
    optinput = []

    #for input_point in input_domain:
    for index in range(len(input_domain)):
        # TODO: better search - N-trees + marching hypercubes ?
        #if abs( function(np.array([input_point])) - targ ) < 1e-4:
        if abs (output_predicted[index] - targ ) < 1e-3:
            optinput.append(input_domain[index])

    return optinput

# define the F()
gem0_helper = ExtCodeHelper()
function = lambda x: gem0_helper.gem0_call_tefltevltegrad_array(x)

# define domain X
x_param = [[400., 2400, 16], [-3600., 0., 16]]
x1 = np.linspace(*x_param[0])
x2 = np.linspace(*x_param[1])
input_domain = np.transpose([np.tile(x1, len(x2)), np.repeat(x2, len(x1))])

# read f(x)
input_distributions = target_distribution(source="../data/AUG_gem_inoutput.txt", target_names=["Te-ft5", "dTi-ft5"]) # for now GEM, get GEM0 data from UQP1 loop

# read f(y)
output_distributions = target_distribution(source="../data/AUG_gem_inoutput.txt", target_names=["flux-Ti-ft5"])

# find Y_pred = Fsurr(X_domain)
mod_folder = '../data/models/'
mod_filename = 'gpr_mod_gem.joblib' # surrogate from GEM0
model_path = os.path.join(mod_folder, mod_filename)
surrogate = load(model_path)
output_predicted = surrogate.predict(input_domain).reshape(-1,1) # TODO: outputs the same

opt_input = qss_input(function, input_domain, input_distributions, output_distributions["flux-Ti-ft5"], output_predicted)

print("Assumed Te and gradTe for quasi-steady state: {}".format(opt_input))
