d3.csv("https://raw.githubusercontent.com/biancaschutz/hroutlook/refs/heads/main/Flourish/flourish_data/hr_defenders_regional_category.csv", function (data) {
    data.forEach(d => d.value = +d.value);

    const cat = d3.nest()
        .key(d => d.Category)
        .entries(data);

    cat.forEach(c => c.values.forEach(v => v.Category = c.key));

    const names = [...new Set(data.map(d => d.name))];
    const values = data.map(d => +d.value);

    const width = 928;
    const height = cat.length * 100;
    const marginTop = 30;
    const marginRight = 10;
    const marginBottom = 10;
    const marginLeft = 200;
    const radius = 7.5;
    const padding = 3;

    const x = d3.scaleLinear()
        .domain([0, d3.max(values)])
        .rangeRound([marginLeft, width - marginRight]);

    const y = d3.scalePoint()
        .domain(cat.map(d => d.key).sort())
        .rangeRound([marginTop, height - marginBottom])
        .padding(1);

    const color = d3.scaleOrdinal()
        .domain(['Central Asia and Southern Asia', 'Eastern Asia and South-eastern Asia',
            'Latin America and the Caribbean', 'Northern America and Europe',
            'Sub-Saharan Africa', 'Western Asia and Northern Africa'])
        .range(['#e69f00', '#56b4e9', '#009e73', '#f0e442', '#d55e00', '#cc79a7'])
        .unknown("#ccc");

    const schemes = {
        "hrd-default": d3.scaleOrdinal()
            .domain(['Central Asia and Southern Asia', 'Eastern Asia and South-eastern Asia',
                'Latin America and the Caribbean', 'Northern America and Europe',
                'Sub-Saharan Africa', 'Western Asia and Northern Africa'])
            .range(['#e69f00', '#56b4e9', '#009e73', '#f0e442', '#d55e00', '#cc79a7']),

        "hrd-lac": d3.scaleOrdinal()
            .domain(['Central Asia and Southern Asia', 'Eastern Asia and South-eastern Asia',
                'Latin America and the Caribbean', 'Northern America and Europe',
                'Sub-Saharan Africa', 'Western Asia and Northern Africa'])
            .range(['#ccc', '#ccc', '#009e73', '#ccc', '#ccc', '#ccc']),

        "hrd-wasia": d3.scaleOrdinal()
            .domain(['Central Asia and Southern Asia', 'Eastern Asia and South-eastern Asia',
                'Latin America and the Caribbean', 'Northern America and Europe',
                'Sub-Saharan Africa', 'Western Asia and Northern Africa'])
            .range(['#ccc', '#ccc', '#ccc', '#ccc', '#ccc', '#cc79a7']),
    };

    const svg = d3.create("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [0, 0, width, height])
        .attr("style", "max-width: 100%; height: auto;");

    var tooltip = d3.select("#chart-container").append("div")
        .attr("class", "hrd-tooltip")
        .style("opacity", 0)
        .style("position", "fixed")
        .style("background-color", "white")
        .style("border", "solid")
        .style("border-width", "1px")
        .style("border-radius", "5px")
        .style("padding", "10px")
        .style("font-family", "Roboto")
        .style("font-size", "14px")
        .style("pointer-events", "none")
        .style("word-wrap", "break-word")
        .style("max-width", "200px")
        .style("z-index", "99999");

    var mousemove = function (d) {
        tooltip
            .html(`In 2025, there were <strong>${d.data.value} confirmed killings</strong> of ${d.data.Category} in ${d.data.name}`)
            .style("left", (d3.event.clientX + 15) + "px")  // clientX pairs with fixed positioning
            .style("top", (d3.event.clientY - 28) + "px");
    }
    var mouseover = function (d) {
        tooltip.transition()
            .duration(0)      // cancel any ongoing fade-out
            .style("opacity", 1);
    }

    var mouseleave = function (d) {
        tooltip
            .transition()
            .duration(500)    // increase delay before hiding
            .style("opacity", 0);
    }

    svg.append("g")
        .attr("transform", `translate(0,${marginTop})`)
        .call(d3.axisTop(x).ticks(null))
        .call(g => g.selectAll("text")
            .style("font-family", "Roboto")
            .style("font-size", "16px")
        )
        .call(g => g.append("text")
            .attr("fill", "currentColor")
            .attr("transform", `translate(${width - marginRight},0)`)
            .attr("text-anchor", "end")
            .attr("dy", -22)
        )
        .call(g => g.selectAll(".tick line")
            .attr("stroke", "#aaa")
            .attr("stroke-opacity", 0.1)    // same as grid lines
            .attr("y2", height - marginBottom)  // extend all the way down like grid lines
        )
        .call(g => g.selectAll(".domain").remove());

    const g = svg.append("g")
        .attr("text-anchor", "end")
        .style("font-family", "Roboto")
        .style("font-size", "16px")
        .selectAll()
        .data(cat)
        .enter().append("g")
        .attr("transform", d => `translate(0,${y(d.key)})`);

    g.append("g")
        .selectAll("circle")
        .data(d => dodge(d.values, { radius: radius * 2 + padding, x: v => x(v.value) }))
        .enter().append("circle")
        .attr("cx", d => d.x)
        .attr("cy", d => d.y)
        .attr("fill", d => color(d.data.name))
        .on("mouseover", mouseover)
        .on("mousemove", mousemove)
        .on("mouseleave", mouseleave)
        .attr("r", radius);

    g.append("foreignObject")
        .attr("x", d => x(d3.min(d.values, v => v.value)) - 156)
        .attr("y", -8)
        .attr("width", 140)
        .attr("height", 40)
        .append("xhtml:div")
        .style("font-family", "Roboto")
        .style("font-size", "16px")
        .style("text-align", "right")
        .style("word-wrap", "break-word")
        .text(d => d.key);

    document.getElementById("chart-container").appendChild(svg.node());

    // legend
    const legend = d3.select("#legend-container")
        .append("div")
        .style("box-sizing", "border-box")
        .style("display", "flex")
        .style("flex-wrap", "wrap")
        .style("gap", "10px")
        .style("padding", "10px 25px 10px 100px")
        .style("font-family", "Roboto")
        .style("font-size", "16px")
        .style("max-width", `${width}px`);

    const hidden = new Set();

    color.domain().forEach(name => {
        const item = legend.append("div")
            .style("display", "flex")
            .style("align-items", "center")
            .style("gap", "6px")
            .style("cursor", "pointer")
            .on("click", function () {
                if (hidden.has(name)) {
                    hidden.delete(name);
                    d3.select(this).style("opacity", 1);
                } else {
                    hidden.add(name);
                    d3.select(this).style("opacity", 0.3);
                }
                svg.selectAll("circle")
                    .style("display", d => hidden.has(d.data.name) ? "none" : null);
            });

        item.append("div")
            .style("width", "15px")
            .style("height", "15px")
            .style("background", color(name))
            .style("flex-shrink", "0");

        item.append("span")
            .text(name);
    });

    // intersection observer for scroll-based color changes
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const scheme = schemes[entry.target.id];
                if (scheme) {
                    svg.selectAll("circle")
                        .transition()
                        .duration(500)
                        .attr("fill", d => scheme(d.data.name));
                }
            }
        });
    }, { threshold: 0.5 });

    ["hrd-default", "hrd-lac", "hrd-wasia"].forEach(id => {
        const el = document.getElementById(id);
        if (el) observer.observe(el);
    });

});