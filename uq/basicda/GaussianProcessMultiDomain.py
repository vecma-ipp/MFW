class def GPR_composed(x_domains):
    
    x_domains = 
    models = 

    # index of domain/model which is considered active now, staefull class
    index_active

    def _init_(self, ):


    def choose_domain(x):
        int i = 0
        for x_domain in x_domains: # TODO binary search for 1D, search on a tree for 2-4D
            if x > x_domain[0] and x <= x_domain[-1]:
                index_active = i
            i = i + 1
                
    def predict(x):
        # Chose and active model and get a prediction for it
        choose_domain(x)
        mean, var = models[dom_index_active].predict(x)
        return mean, var

    def get_active_hyperparams():
        params_active = models[index_active].get_params()
        kernel_active = models[index_active].kernel
        theta_active = kernel_active.theta

    
