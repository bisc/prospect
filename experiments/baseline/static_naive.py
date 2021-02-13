import torch
import pyro
import sys
import csv

# command line takes one argument for number of samples
if len(sys.argv) >= 2:
    num_samples = int(sys.argv[1])
else:
    print('In the command line, please specify number of samples\n')
    sys.exit()

def static_ex():
    # encode specifications
    # time and lane are independent given detection
    prob_detected_given_day = .75
    prob_detected_given_twilight = .4
    prob_detected_given_night = .2
    prob_day = .6
    prob_twilight = (1 - prob_day) / 2
    prob_night = prob_twilight
    prob_out = .2
    prob_detected_given_in = .6
    
    # create cat dist for time var, sample
    time = pyro.sample('time', pyro.distributions.Categorical(torch.tensor([prob_day, prob_twilight, prob_night])))
    
    # update probability of detected given observed time
    if(time.item() == 0.0):
        time = 'day'
        detection = pyro.sample('detection', pyro.distributions.Bernoulli(prob_detected_given_day))
    elif(time.item() == 1.0):
        time = 'twilight'
        detection = pyro.sample('detection', pyro.distributions.Bernoulli(prob_detected_given_twilight))
    else:
        time = 'night'
        detection = pyro.sample('detection', pyro.distributions.Bernoulli(prob_detected_given_night))
    
    if(detection.item() == 1.0):
        detection = 'detected'
    else:
        detection = 'not detected'
    
    # if one isn't careful, it may be tempting to generate data for "lane" using just the marginal probability P[lane = "out"] = .2, as shown below
    lane = pyro.sample('lane', pyro.distributions.Bernoulli(1 - prob_out))
    if(lane.item() == 1.0):
        lane = 'in'
    else:
        lane = 'out'

    # This method uses the assumption that lane and detection are independent, but this is not necessarily true based on the specifications.
    # More calculation is needed to compute conditional probabilities as detailed in the static.py.
        
    return [time, detection, lane]

with open('static_baseline_naive.csv', 'w') as file:
    writer = csv.writer(file)
    # writer.writerow(['time', 'detection', 'lane'])
    for i in range(1, num_samples + 1):
        writer.writerow(static_ex())
