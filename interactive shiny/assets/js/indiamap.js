var width = 960,
height = 600;
var color= d3.scale.ordinal()
.domain([1,2,3,4,5,6,7,8,9])
.range("red", "steelblue");

var projection = d3.geo.mercator().scale(1100).translate([-1000,800]);
var path = d3.geo.path()
.projection(projection);

var root = svg 
.attr("viewBox", "0 0 900 800")
.attr("preserveAspectRatio", "xMidYMid meet");
var data;
r2d3.onRender(function(us, svg, width, height, options) {
  
  
  var cantons = topojson.feature(us, us.objects.india);
  
  //svg.call(tip);
  var group=root.selectAll("g")
  .data(cantons.features)
  .enter()
  .append("g");
  //.on('mouseover', tip.show)
  //.on('mouseout', tip.hide)
  
  
  var tip = d3.tip()
  .attr('class', 'd3-tip')
  .offset([-5, 0])
  .style("left", "300px")
  .style("top", "400px")
  .html(function(d) {
    return ("<a href="+d.nam+" target='_blank'>"+d.name +"</a>");
  });
  
  root.call(tip);
  
  
  root.selectAll(".pin")
  .data(us.values)
  .enter().append("circle", ".pin")
  .attr("r", 5)
  .attr("transform", function(d) {
    return "translate(" + projection([
      d.decimalLongitude,
      d.decimalLatitude
      ]) + ")";
  })
  .on('mouseover', tip.show)
  .on('click', tip.hide);		
  
  //var projection = d3.geo.mercator().scale(900).translate([-600,700]);
  var path= d3.geo.path().projection(projection);
  
  var areas= group.append("path")
  .attr("d", path)
  .attr("class", "area")
  .attr("fill","steelblue");
  
});