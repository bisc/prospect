import torch
import pyro
import sys

# command line takes one argument for number of samples
if len(sys.argv) >= 2:
    num_samples = int(sys.argv[1])
else:
    print('In the command-line, please specify number of samples\n')
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
    
    # the time and lane variables are conditionally independent given any value of the detection variable; suffices to compute P[lane = 'in' | detection = 'detected'] and P[lane = 'in' | detection = 'not detected'] to generate data
    
    # compute P[detected] before invoking Bayes' Theorem
    prob_detected = prob_detected_given_day * prob_day + prob_detected_given_twilight * prob_twilight + prob_detected_given_night * prob_night
    
    if(detection.item() == 1.0):
        detection = 'detected'
        prob_in_given_detected = prob_detected_given_in * (1 - prob_out) / prob_detected
        lane = pyro.sample('lane', pyro.distributions.Bernoulli(prob_in_given_detected))
    else:
        detection = 'not detected'
        prob_in_given_notdetected = ((1 - prob_out) - (prob_detected_given_in * (1 - prob_out))) / (1 - prob_detected)
        lane = pyro.sample('lane', pyro.distributions.Bernoulli(prob_in_given_notdetected))
    
    if(lane.item() == 1.0):
        lane = 'in'
    else:
        lane = 'out'
        
    return time, detection, lane

for i in range(num_samples):
    print('Sample ' + str(i + 1) + ': ')
    print(static_ex())
    print('\n')
