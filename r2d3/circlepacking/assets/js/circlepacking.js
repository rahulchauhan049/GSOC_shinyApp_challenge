// !preview r2d3 data = read.csv("data1.csv"), d3_version = 4

// Based on: https://bl.ocks.org/mbostock/4063530

var diameter = Math.min(width, height),
    g = svg.append("g").attr(
      "transform",
      "translate(" + (width - diameter) / 2 + "," + (height - diameter) / 2 + ")"),
    format = d3.format(",d");
var stratify = d3.stratify()

    .parentId(function (d) {
        return d.id.substring(0, d.id.lastIndexOf("."));
    });
var pack = d3.pack()
    .size([diameter - 4, diameter - 4]);

r2d3.onRender(function(data, svg, width, height, options) {
  var root = stratify(data);
  
root.sum(function (d) {
    return +d.value;
})
.sort(function (a, b) {
    return b.height - a.height || b.value - a.value;
});

  var node = g.selectAll(".node")
    .data(pack(root).descendants())
    .enter().append("g")
      .attr("class", function(d) { return d.children ? "node" : "leaf node"; })
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

  node.append("title")
      .text(function(d) { return d.data.id + "\n" + format(d.value); });

  node.append("circle")
      .attr("r", function(d) { return d.r; });

  node.filter(function(d) { return !d.children; }).append("text")
      .attr("dy", "0.3em")
      .attr("font-size", Math.max(width, height) / 100)
      .text(function(d) {
        var parts    = d.data.id.split('.');
        var answer   = parts[parts.length - 1];
        return answer.substring(0, d.r / 3)
      })
      
});