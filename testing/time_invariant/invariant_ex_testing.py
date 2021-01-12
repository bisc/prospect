import pyro
import sys
import csv

# command line takes one argument for number of time steps in the time series
if len(sys.argv) >= 2:
    num_steps_arg = int(sys.argv[1])
else:
    print('In the command line, please specify number of time steps in the time series\n')
    sys.exit()

def invariant_ex(num_steps):
    if (num_steps < 1):
       return []   
    # encode specifications
    # latency and ping_tmin1 are independent given ping_t
    prob_t_hi_given_tmin1_hi = .7
    prob_t_lo_given_tmin1_lo = .65
    prob_lat_lo = .8
    prob_t_hi_given_lat_hi = .6

    # invariance assumption: p(ping_t = hi) = p(ping_t-1 = hi)

    prob_t_hi = (1 - prob_t_lo_given_tmin1_lo) / (1 - prob_t_hi_given_tmin1_hi + (1 - prob_t_lo_given_tmin1_lo)) # derive by hand using invariance assumption    

    # solve for p(latency_t = hi | ping_t = hi) and p(latency_t = hi | ping_t = lo)

    prob_lat_hi_given_t_hi = prob_t_hi_given_lat_hi * (1 - prob_lat_lo) / prob_t_hi
    prob_lat_hi_given_t_lo = (1 - prob_t_hi_given_lat_hi) * (1 - prob_lat_lo) / (1 - prob_t_hi)

    return_list = [['row', 'latency', 'ping']] # add results from each iteration to this list

    # sample initial ping value from marginal prob
    ping_curr = pyro.sample('ping_prev', pyro.distributions.Bernoulli(prob_t_hi))

    if (ping_curr.item() == 1.0):
        ping_curr = 'high'
        lat_curr = pyro.sample('lat_curr', pyro.distributions.Bernoulli(prob_lat_hi_given_t_hi))
    else:
        ping_curr = 'low'
        lat_curr = pyro.sample('lat_curr', pyro.distributions.Bernoulli(prob_lat_hi_given_t_lo))

    if (lat_curr.item() == 1.0):
        lat_curr = 'high'
    else:
        lat_curr = 'low'

    return_list.append([1, lat_curr, ping_curr])
    ping_prev = ping_curr

    for x in range(2, num_steps + 1):
        if (ping_prev == 'high'):
            ping_curr = pyro.sample('ping_curr', pyro.distributions.Bernoulli(prob_t_hi_given_tmin1_hi))
        else:
            ping_curr = pyro.sample('ping_curr', pyro.distributions.Bernoulli(1 - prob_t_lo_given_tmin1_lo))
        if (ping_curr.item() == 1.0):
            ping_curr = 'high'
            lat_curr = pyro.sample('lat_curr', pyro.distributions.Bernoulli(prob_lat_hi_given_t_hi))
        else:
            ping_curr = 'low'
            lat_curr = pyro.sample('lat_curr', pyro.distributions.Bernoulli(prob_lat_hi_given_t_lo))
        if (lat_curr.item() == 1.0):
            lat_curr = 'high'
        else:
            lat_curr = 'low'
        return_list.append([x, lat_curr, ping_curr])
        ping_prev = ping_curr
    return return_list

with open('invariant_ex_testing.csv', 'w') as file:
    writer = csv.writer(file)
    writer.writerows(invariant_ex(num_steps_arg))