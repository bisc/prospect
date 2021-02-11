import pyro
import sys
import csv

# command line takes two arguments for number of time steps in the time series and instances of time series
if len(sys.argv) >= 3:
    num_steps_arg = int(sys.argv[1])
    num_instances_arg = int(sys.argv[2])
else:
    print('In the command line, please specify number of time steps and number of time series (in that order) in the time series\n')
    sys.exit()

def variant_ex(num_steps):
    if (num_steps < 1):
       return []   
    
    # encode specifications

    prob_tool_broken_given_toolmin1_broken = 1
    prob_op_ok = .8
    
    # Keep below specifications in mind:
    # other specs: operation and tool_tmin1 are independent
    # P[tool_t = "func"] = P[tool_t-1 = "func"]-.1*P[tool_t-1 = "func"]
    # P[operation_t = "ok" && tool_t = "func"] = .95*P[operation_t = "ok" && tool_t-1 = "func"]

    prob_toolmin1_func = .9 # base case, step 0

    return_tool_list = [] # values of tool at each time step of a single time series instance
    return_op_list = [] # values of operation at each time step of an instance

    # sample initial tool value from marginal prob
    tool_prev = pyro.sample('tool_prev', pyro.distributions.Bernoulli(prob_toolmin1_func))

    # label initial tool value
    if (tool_prev.item() == 1.0):
        tool_prev = 'func'
    else:
        tool_prev = 'broken'

    for x in range(num_steps):
        # intermediate computations done each iteration
        prob_tool_func = prob_toolmin1_func - .1 * prob_toolmin1_func
        prob_tool_broken = 1 - prob_tool_func
        prob_toolmin1_broken = 1 - prob_toolmin1_func

        # compute p(tool_t = func | tool_t-1 = broken)
        prob_tool_func_given_toolmin1_broken = (prob_toolmin1_broken - (prob_tool_broken_given_toolmin1_broken * prob_toolmin1_broken)) / prob_toolmin1_broken
        
        # compute p(tool_t = func | tool_t-1 = func)
        prob_tool_func_given_toolmin1_func = (prob_tool_func - (prob_tool_func_given_toolmin1_broken * prob_toolmin1_broken)) / prob_toolmin1_func

        # compute current step probabilities
        # prob_op_ok_and_tool_func = .95 * prob_op_ok * prob_toolmin1_func # use independence fact to split RHS
        # prob_op_ok_given_tool_func = prob_op_ok_and_tool_func / prob_tool_func
        # prob_op_ok_given_tool_broken = (prob_op_ok - prob_op_ok_and_tool_func) / prob_tool_broken

        # sample tool_curr given tool_prev
        if (tool_prev == 'func'):
            tool_curr = pyro.sample('tool_curr', pyro.distributions.Bernoulli(prob_tool_func_given_toolmin1_func))
        else:
            tool_curr = pyro.sample('tool_curr', pyro.distributions.Bernoulli(prob_tool_func_given_toolmin1_broken))

        # label tool_curr
        if (tool_curr.item() == 1.0):
            tool_curr = 'func'
            # op_curr = pyro.sample('op_curr', pyro.distributions.Bernoulli(prob_op_ok_given_tool_func))
        else:
            tool_curr = 'broken'
            # op_curr = pyro.sample('op_curr', pyro.distributions.Bernoulli(prob_op_ok_given_tool_broken))

        # sample op_curr from marginal probability
        op_curr = pyro.sample('op_curr', pyro.distributions.Bernoulli(prob_op_ok))

        # label op_curr
        if (op_curr.item() == 1.0):
            op_curr = 'ok'
        else:
            op_curr = 'fail'
        
        return_tool_list.append(tool_curr)
        return_op_list.append(op_curr)

        # update tool_t-1 with tool_t
        tool_prev = tool_curr
        prob_toolmin1_func = prob_tool_func

        return_lists = [return_tool_list, return_op_list]
    return return_lists

with open('time_variant_baseline_var_tool_naive.csv', 'w') as tool_file, open('time_variant_baseline_var_op_naive.csv', 'w') as op_file:
    tool_writer = csv.writer(tool_file)
    op_writer = csv.writer(op_file)
    if (num_instances_arg >= 1):
        for y in range(num_instances_arg):
            result = variant_ex(num_steps_arg)
            tool_writer.writerow(result[0])
            op_writer.writerow(result[1])