const express = require("express");
const shell = require('shelljs');
const PORT = process.env.PORT || 5000;  //see below
const app = new express();

function escapeShellArg(arg) {
    return `'${arg.replace(/'/g, `'\\''`)}'`;
}

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

app.get("/api/prospect", (req, res) => {
    var input = req.query.input;
    var command = "./script/PROSPECT_string_input.wls "
    var final_input = command + JSON.stringify(input);
    var output = shell.exec(final_input);

    res.status(200).send(output);
});

app.listen(PORT, () => console.log(`listening on port ${PORT}`));