const { exec } = require("child_process");
const shell = require('shelljs');

function ProspectRun(textInput: string) {
    var commandStart = "./PROSPECT_string_input.wls ";
    var commandEnd = "\"" + textInput + "\"";
    var command = commandStart.concat(commandEnd);
    // shell.config.execPath = shell.which('node').toString();
    var extra = shell.exec("ls");
    console.log(extra);
    return command;
}

export default ProspectRun;