let parseUntil = (s, until) => {
  let l1 = String.length(s);
  let l2 = String.length(until);
  let rec loop = (i, j, left) =>
    if (j >= l2) {
      Some((
        left,
        String.sub(
          s,
          String.length(left) + String.length(until),
          String.length(s) - String.length(left) - String.length(until),
        ),
      ));
    } else if (i >= l1) {
      None;
    } else if (s.[i] == until.[j]) {
      loop(i + 1, j + 1, left);
    } else {
      let extra =
        if (j > 0) {
          String.sub(s, i - j, j);
        } else {
          "";
        };
      loop(i + 1, 0, left ++ extra ++ String.make(1, s.[i]));
    };
  loop(0, 0, "");
};

let eatSpaces = line => {
  let rec loop = count =>
    if (count >= String.length(line) || line.[count] != ' ') {
      String.sub(line, count, String.length(line) - count);
    } else {
      loop(count + 1);
    };
  loop(0);
};

let findBackwards = (line, c) => {
  let rec loop = i =>
    if (i < 0) {
      line;
    } else if (line.[i] == c) {
      String.sub(line, i + 1, String.length(line) - i - 1);
    } else {
      loop(i - 1);
    };

  loop(String.length(line) - 1);
};

let packageName = if (Array.length(Sys.argv) > 1) {
  let url = Sys.argv[1];
  let packageName = findBackwards(url, '/');
  print_endline("Fetching `" ++ packageName ++ "`");
  let err = Sys.command("git clone " ++ url);
  if (err != 0) {
    exit(err);
  };
  packageName;
} else {
  ".";
};

let ic = open_in(packageName ++ "/package.json");
let ob = Buffer.create(100);
let running = ref(true);
while (running^) {
  switch (input_line(ic)) {
  | exception End_of_file =>
    close_in(ic);
    running := false;
  | line =>
    switch (parseUntil(line, "github:schmavery/reprocessing"), parseUntil(line, "schmavery/reprocessing"), parseUntil(line, "github:Schmavery/reprocessing")) {
    | (None, None, None) =>
      switch (parseUntil(line, "bs-platform\":")) {
      | None => Buffer.add_string(ob, line)
      | Some((before, after)) =>
        /* Eat space and quote, then grab everything until the next quote */
        let after = eatSpaces(after);
        switch (
          parseUntil(String.sub(after, 1, String.length(after) - 1), "\"")
        ) {
        | None => assert(false)
        | Some((_, endOfLine)) =>
          Buffer.add_string(ob, before);
          Buffer.add_string(ob, "bsb-native\": \"4.0.6\"");
          Buffer.add_string(ob, endOfLine);
        };
      }
    | (Some((before, after)), Some(_), None)
    | (None, Some((before, after)), None)
    | (None, None, Some((before, after))) =>
      switch (parseUntil(before, "Reprocessing")) {
      | None => Buffer.add_string(ob, before)
      | Some((sl, el)) =>
        Buffer.add_string(ob, sl);
        Buffer.add_string(ob, "reprocessing");
        Buffer.add_string(ob, el);
      };
      Buffer.add_string(ob, "0.2.0");
      Buffer.add_string(ob, after);
    | _ => assert(false)
    };
    Buffer.add_char(ob, '\n');
  };
};

let oc = open_out(packageName ++ "/package.json");
Buffer.output_buffer(oc, ob);
close_out(oc);

let ic = open_in(packageName ++ "/bsconfig.json");
let ob = Buffer.create(100);
let running = ref(true);
while (running^) {
  switch (input_line(ic)) {
  | exception End_of_file =>
    close_in(ic);
    running := false;
  | line =>
    switch (parseUntil(line, "Reprocessing")) {
    | None => Buffer.add_string(ob, line)
    | Some((sl, el)) =>
      Buffer.add_string(ob, sl);
      Buffer.add_string(ob, "reprocessing");
      Buffer.add_string(ob, el);
    };
    Buffer.add_char(ob, '\n');
  };
};

let oc = open_out(packageName ++ "/bsconfig.json");
Buffer.output_buffer(oc, ob);
close_out(oc);

let exitCode =
  Sys.command(
    "cd " ++ packageName ++ " && npm install && npm run build && npm run start",
  );
if (exitCode != 0) {
  failwith("Got exit code: " ++ string_of_int(exitCode));
};

Sys.command("cd " ++ packageName ++ " && git diff bsconfig.json package.json");
Sys.command("cd " ++ packageName ++ " && git add bsconfig.json package.json && git commit -m \"Upgrade to latest release of reprocessing and bsb-native\" && git remote add fork https://github.com/bsansouci/" ++ packageName);
Sys.command("cd " ++ packageName ++ " && git push fork master");
