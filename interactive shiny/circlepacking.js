// !preview r2d3 data = read.csv("src/csv/data1.csv"), d3_version = 5

// Based on: https://bl.ocks.org/mbostock/4063530
var color = d3.scaleLinear()
    .domain([0, 5])
    .range(["hsl(152,80%,80%)", "hsl(228,30%,40%)"])
    .interpolate(d3.interpolateHcl)
var format = d3.format(",d");

    svg
      .style("cursor", "pointer")   
      .on("click", () => zoom(root));
      
var stratify = d3.stratify()
    .parentId(function(d) { return d.id.substring(0, d.id.lastIndexOf(".")); });

var diameter = Math.min(width, height),
    g = svg.append("g").attr(
      "transform",
      "translate(" + (width)/2 + "," + (height - diameter+40) *5 + ")")
      

var pack = d3.pack()
    .size([diameter - 4, diameter - 4]);

r2d3.onRender(function(data, svg, width, height, options) {
  var root = stratify(data);
  
root.sum(function (d) {
    return d.value;
})
.sort(function (a, b) {
    return b.height - a.height || b.value - a.value;
});

  var node = g.selectAll(".node")
    .data(pack(root).descendants())
    .enter()
    .append("circle")
          .attr("fill", d => d.children ? color(d.depth) : "white")
      .attr("pointer-events", d => !d.children ? "none" : null)
      .attr("d", function(d) { return d.data.id; })
      .on("mouseover", function() { d3.select(this).attr("stroke", "#000"); })
      .on("mouseout", function() { d3.select(this).attr("stroke", null); })
      .on("click", function(d) {focus !== d && (zoom(d), d3.event.stopPropagation())
        Shiny.setInputValue(
        "bar_clicked", 
        d3.select(this).attr("d"),
        {priority: "event"}
        );
      });
      
     

    var label = g.append("g")
      .style("font", "10px sans-serif")
      .attr("pointer-events", "none")
      .attr("text-anchor", "middle")
    .selectAll(".label")
    .data(root.descendants()).enter()
    .append("text")
      .style("fill-opacity", d => d.parent === null ? 1 : 0)
      .style("display", d => d.parent === null ? "inline" : "none")
      .text(function(d) {
        var parts    = d.id.split('.');
        var answer   = parts[parts.length - 1];
        return answer
      });
       node.append("title")
      .text(function(d) { return d.id.split(".").pop() + "\n" + format(d.value); });

  zoomTo([root.x, root.y, root.r*5]);
  function zoomTo(v) {
    const k = width / v[2];

    view = v;

    label.attr("transform", d => `translate(${(d.x - v[0]) * k},${(d.y - v[1]) * k})`);
    node.attr("transform", d => `translate(${(d.x - v[0]) * k},${(d.y - v[1]) * k})`);
    node.attr("r", d => d.r * k);
  }

  function zoom(d) {
    const focus0 = focus;

    focus = d;

    const transition = svg.transition()
        .duration(d3.event.altKey ? 7500 : 750)
        .tween("zoom", d => {
          const i = d3.interpolateZoom(view, [focus.x, focus.y, focus.r * 5]);
          return t => zoomTo(i(t));
        });

    label
      .filter(function(d) { return d.parent === focus || this.style.display === "inline"; })
      .transition(transition)
        .style("fill-opacity", d => d.parent === focus ? 1 : 0)
        .on("start", function(d) { if (d.parent === focus) this.style.display = "inline"; })
        .on("end", function(d) { if (d.parent !== focus) this.style.display = "none"; });
  }

});