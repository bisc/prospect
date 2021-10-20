const express = require("express");
const shell = require('shelljs');
const PORT = process.env.PORT || 5000;  //see below
const app = new express();

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

app.get("/api/prospect", (req, res) => {
    var input = req.query.input;
    var command = "./script/PROSPECT_string_input.wls "
    // console.log(command + "\"" + input + "\"");
    var output = shell.exec(command + "\"" + input + "\"");
    console.log(output);
    // var command = "./script/PROSPECT.wls script/testing/static/Static.txt"
    // var command = "./script/PROSPECT_string_input.wls \"\"casetype: \"timeinvariant\"\nvariables: {latency, ping}\nvalues: {{\"low\", \"high\"}, {\"low\", \"high\"}}\ntimesteps: {0, 1}\nnumsamples: 100\n\nindependence\ncondindep[{latency[t], ping[t-1]}, {ping[t]}]\n\nmain\nP[ping[t] = \"high\" | ping[t-1] = \"high\"] = .7\nP[ping[t] = \"low\" | ping[t-1] = \"low\"] = .65\nP[latency[t] = \"low\"] = .8\nP[ping[t] = \"high\" | latency[t] = \"high\"] = .6\"";
    // var output = shell.exec(command);
    // console.log(output);

    res.status(200).send(output);
});

app.listen(PORT, () => console.log(`listening on port ${PORT}`));