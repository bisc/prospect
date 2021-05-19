const express = require('express');
const { exec } = require("child_process");
const shell = require('shelljs');
const app = new express();
const fs = require('fs');

const port = 3000

app.use(express.urlencoded({
  extended: true
}))

app.get('/', function(req, res) {
  res.sendFile('HTML/home.html', { root: __dirname });
  fs.writeFile("demo.txt", "", function(err, result) {
   if(err) console.log('error', err);
  });

    // if (!req.inspec) {
    //   res.sendFile('HTML/home.html', { root: __dirname });
    //   fs.writeFile("DEMO.TXT", "", function(err, result) {
    //    if(err) console.log('error', err);
    //   });
    // } else {
    //   res.sendFile('HTML/home.html', { root: __dirname });
    //   fs.writeFile("DEMO.TXT", "Baloney", function(err, result) {
    //    if(err) console.log('error', err);
    //   });
    // }
    // exec("ls", (error, stdout, stderr) => {
    //     if (error) {
    //         console.log(`error: ${error.message}`);
    //         return;
    //     }
    //     if (stderr) {
    //         console.log(`stderr: ${stderr}`);
    //         return;
    //     }
    //     console.log(`stdout: ${stdout}`);
    // });
    // shell.exec('./stest.wls')

});

app.post('/run',function(req,res){
   var allText = req.body.inspec;
   // res.send(allText);
   // console.log(allText);
   console.log(allText);

   fs.writeFile("demo.txt", allText, function(err, result) {
    if(err) console.log('error', err);
   });

  var part1 = "<!DOCTYPE html> \
  <html> \
  <body> \
  \
  <h1>Running Headline</h1> \
  <p>";

  var part2 = "</p> \
  <p><a href="/">Return to home</a></p> \
 \
  </body> \
  </html>";

  var all_html = part1.concat(part2);

   // Run PROSPECT
   fs.writeFile("HTML/run_custom.html", all_html, function(err, result) {
    if(err) console.log('error', err);
   });

   res.sendFile('HTML/run.html', { root: __dirname });
   // when calling this, needs to send output of prospect to HTML
   // Make a custom HTML to do this
});

// not in use right now
app.get('/run', function(req, res) {
    res.sendFile('HTML/run.html', { root: __dirname });
    // exec("ls", (error, stdout, stderr) => {
    //     if (error) {
    //         console.log(`error: ${error.message}`);
    //         return;
    //     }
    //     if (stderr) {
    //         console.log(`stderr: ${stderr}`);
    //         return;
    //     }
    //     console.log(`stdout: ${stdout}`);
    // });

    var filename = req.query.f;
    if (!filename) {
      filename = 'StaticEx';
    }
    var command = './PROSPECT.wls testing/static/' + filename + '.txt'
    var output_text = shell.exec(command)

    // Writing it to a file
    fs.writeFile("demo.txt", output_text, function(err, result) {
     if(err) console.log('error', err);
    });

});

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})
