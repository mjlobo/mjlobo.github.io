// fetch("./tables/time_all.html")
//   .then(res => {
//     return res.text();
//   })
//   .then(html => {
//     document.body.innerHTML = html;
//   });

let globalResultsDiv = document.getElementById("global_results");
let subResultsDiv = document.getElementById("subresults");
let columnsGlobal = document.createElement("div");
columnsGlobal.className = "columns";
columnsGlobal.appendChild(
  createColumnWithImage("./plots/plot_correct_all.png")
);
columnsGlobal.appendChild(
  createColumnWithImage("./plots/plot_correct_diffs_all.png")
);
columnsGlobal.appendChild(createColumnWithImage("./plots/plot_time_all.png"));
columnsGlobal.appendChild(
  createColumnWithImage("./plots/plot_time_diffs_all.png")
);

globalResultsDiv.appendChild(columnsGlobal);

let columnsTablesGlobal = document.createElement("div");
columnsTablesGlobal.className = "columns";
columnsTablesGlobal.appendChild(createTable("./tables/correct_all"));
columnsTablesGlobal.appendChild(createTable("./tables/correct_diffs_all"));
columnsTablesGlobal.appendChild(createTable("./tables/time_all"));
columnsTablesGlobal.appendChild(createTable("./tables/time_diffs_all"));
globalResultsDiv.appendChild(columnsTablesGlobal);

// let names = [
//   "easy_one",
//   "easy_two",
//   "medium_one",
//   "medium_two",
//   "hard_one",
//   "hard_two"
// ];
let names = new Map();
names.set("easy_one", "One county, easy");
names.set("easy_two", "Two counties, easy");
names.set("medium_one", "One county, medium");
names.set("medium_two", "Two counties, medium");
names.set("hard_one", "One county, hard");
names.set("hard_two", "Two counties, hard");
let fileNames = ["correct_", "correct_diffs_", "time_", "time_diffs_"];
//for (let name of names) {
names.forEach((printName, name) => {
  let title = document.createElement("h4");
  title.className = "title is-5";
  title.innerHTML = printName;
  subResultsDiv.appendChild(title);
  let divider = document.createElement("hr");
  divider.className = "solid";
  subResultsDiv.appendChild(divider);
  let columnsPlots = document.createElement("div");
  columnsPlots.className = "columns";
  for (let fileName of fileNames) {
    columnsPlots.appendChild(
      createColumnWithImage("./plots/plot_" + fileName + name + ".png")
    );
  }

  let columnsTables = document.createElement("div");
  columnsTables.className = "columns";
  for (let fileName of fileNames) {
    columnsTables.appendChild(createTable("./tables/" + fileName + name));
  }

  subResultsDiv.appendChild(columnsPlots);
  subResultsDiv.appendChild(columnsTables);
});

function createColumnWithImage(path) {
  let column = document.createElement("div");
  column.className = "column";
  let figureElement = document.createElement("figure");
  let imageElement = document.createElement("img");
  imageElement.src = path;
  figureElement.appendChild(imageElement);
  column.appendChild(figureElement);
  return column;
}

function createTable(path) {
  let column = document.createElement("div");
  column.className = "column";
  let tableElement = document.createElement("table");
  tableElement.className = "table is-narrow is-fullwidth";

  fetch(path)
    .then(res => {
      return res.text();
    })
    .then(html => {
      tableElement.innerHTML = html;
    });

  column.appendChild(tableElement);
  return column;
}
