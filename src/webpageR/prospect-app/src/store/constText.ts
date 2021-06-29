export const staticExample =  "casetype: \"static\"\nvariables: {time, detection, lane}\nvalues: {{\"day\", \"twilight\", \"night\"}, {\"detected\", \"not detected\"}, {\"out\", \"in\"}}\nnumsamples: 100\n\nindependence\ncondindep[{time, lane}, {detection}]\n\nmain\nP[detection = \"detected\" | time = \"day\"] = .75\nP[detection = \"detected\" | time = \"twilight\"] = .4\nP[detection = \"detected\" | time = \"night\"] = .2\nP[time = \"day\"] = .6\nP[time = \"twilight\"] = P[time = \"night\"]\nP[lane = \"out\"] = .2\nP[detection = \"detected\" | lane = \"in\"] = .6";

export const timeInvariantExample = "casetype: \"timeinvariant\"\nvariables: {latency, ping}\nvalues: {{\"low\", \"high\"}, {\"low\", \"high\"}}\ntimesteps: {0, 1}\nnumsamples: 100\n\nindependence\ncondindep[{latency[t], ping[t-1]}, {ping[t]}]\n\nmain\nP[ping[t] = \"high\" | ping[t-1] = \"high\"] = .7\nP[ping[t] = \"low\" | ping[t-1] = \"low\"] = .65\nP[latency[t] = \"low\"] = .8\nP[ping[t] = \"high\" | latency[t] = \"high\"] = .6";

export const timeVariantExample = "casetype: \"timevariant\"\nvariables: {tool, operation}\nvalues: {{\"broken\", \"func\"}, {\"fail\", \"ok\"}}\ntimesteps: {1, 0}\nnumsamples: 100\n\nindependence\nindep[{operation[t], tool[t-1]}]\n\nbasecase\nP[tool[0] = \"func\"] = .9\n\nmain\nP[tool[t] = \"broken\" | tool[t-1] = \"broken\"] = 1\nP[operation[t] = \"ok\"] = .8\nP[tool[t] = \"func\"] = .9*P[tool[t-1] = \"func\"]\nP[operation[t] = \"ok\" && tool[t] = \"func\"] = .95*P[operation[t] = \"ok\" && tool[t-1] = \"func\"]";