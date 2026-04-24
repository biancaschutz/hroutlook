d3.csv("https://raw.githubusercontent.com/biancaschutz/hroutlook/refs/heads/main/data/hr_defenders_regional_category.csv", function (data) {
    data.forEach(d => d.value = +d.value);

    const cat = d3.nest()
        .key(d => d.Category)
        .entries(data);

    cat.forEach(c => c.values.forEach(v => v.Category = c.key));

    const names = [...new Set(data.map(d => d.name))];
    const values = data.map(d => +d.value);

    const isMobile = window.innerWidth <= 991;

    const width = isMobile ? window.innerWidth - 20 : 928;
    const marginTop = 30;
    const marginRight = 10;
    const marginBottom = 10;
    const marginLeft = isMobile ? 75 : 200;
    const height = cat.length * (isMobile ? 80 : 125);
    const radius = isMobile ? 4 : 7.5;
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
        .range(['#e69f00', '#56b4e9', '#009e73', '#f0e442', '#d55e00', '#cc79a7']);

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

    var tooltip = d3.select("#chart-container-hrd").append("div")
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
            .html(`In 2025, there were <strong>${d.value} confirmed killings</strong> of ${d.Category} in ${d.name}`);

        const ttWidth = tooltip.node().offsetWidth;
        const ttHeight = tooltip.node().offsetHeight;
        const mouseX = d3.event.clientX;
        const mouseY = d3.event.clientY;
        const vw = window.innerWidth;
        const vh = window.innerHeight;

        // horizontal: prefer left, flip right if not enough room
        let left;
        if (mouseX - ttWidth - 15 < 0) {
            // not enough room on left, go right
            left = mouseX + 15;
        } else {
            left = mouseX - ttWidth - 15;
        }

        // if going right would also overflow, clamp to viewport edge with padding
        if (left + ttWidth > vw) {
            left = vw - ttWidth - 10;
        }

        // vertical: flip up if tooltip would go below viewport
        let top;
        if (mouseY - 28 + ttHeight > vh) {
            top = mouseY - ttHeight - 10;
        } else {
            top = mouseY - 28;
        }

        tooltip
            .style("left", left + "px")
            .style("top", top + "px");
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
            .style("font-size", isMobile ? "11px" : "16px")
        )
        .call(g => g.append("text")
            .attr("fill", "currentColor")
            .attr("transform", `translate(${width - marginRight},0)`)
            .attr("text-anchor", "end")
            .attr("dy", -22)
        )
        .call(g => g.selectAll(".tick line")
            .attr("stroke", "#aaa")
            .attr("stroke-opacity", 0.25)
            .attr("y2", height - marginBottom)
        )
        .call(g => g.selectAll(".domain").remove());

    const g = svg.append("g")
        .attr("text-anchor", "end")
        .style("font-family", "Roboto")
        .style("font-size", isMobile ? "11px" : "16px")
        .selectAll()
        .data(cat)
        .enter().append("g")
        .attr("transform", d => `translate(0,${y(d.key)})`);

    g.append("g")
        .selectAll("circle")
        .data(d => {
            // Group values by their x-position, then assign alternating offsets
            const byValue = d3.nest().key(v => v.value).entries(d.values);
            byValue.forEach(group => {
                group.values.forEach((v, i) => {
                    const half = (group.values.length - 1) / 2;
                    v.__xOffset = (i - half) * (radius * 2);
                });
            });
            return d.values;
        })
        .enter().append("circle")
        .attr("cx", d => x(d.value) + d.__xOffset)
        .attr("cy", 0)
        .attr("fill", d => color(d.name))
        .on("mouseover", mouseover)
        .on("mousemove", mousemove)
        .on("mouseleave", mouseleave)
        .attr("r", radius);

    g.append("foreignObject")
        .attr("x", 0 - (isMobile ? 5 : 10))
        .attr("y", -8)
        .attr("width", isMobile ? 80 : 140)
        .attr("height", 40)
        .append("xhtml:div")
        .style("font-family", "Roboto")
        .style("font-size", isMobile ? "11px" : "16px")
        .style("text-align", "right")
        .style("word-wrap", "break-word")
        .text(d => d.key);

    document.getElementById("chart-container-hrd").appendChild(svg.node());

    // legend
    const legend = d3.select("#legend-container-hrd")
        .append("div")
        .style("box-sizing", "border-box")
        .style("display", "flex")
        .style("flex-wrap", "wrap")
        .style("gap", "10px")
        .style("font-family", "Roboto")
        .style("font-size", isMobile ? "12px" : "16px")
        .style("padding", isMobile ? "10px" : "10px 25px 10px 100px")
        .style("max-width", `${width}px`);

    const hidden = new Set();

    color.domain().forEach(name => {

        // Legend item container
        const item = legend.append("div")
            .datum(name)                     // bind series name
            .attr("class", "legend-item")    // so opacity only applies to this
            .style("display", "flex")
            .style("align-items", "center")
            .style("gap", "6px")
            .style("cursor", "pointer")
            .style("user-select", "none")    // prevent text selection
            .on("click", function () {
                if (hidden.has(name)) {
                    hidden.delete(name);
                    d3.select(this).style("opacity", 1);
                } else {
                    hidden.add(name);
                    d3.select(this).style("opacity", 0.3);
                }

                svg.selectAll("circle")
                    .style("display", d => hidden.has(d.name) ? "none" : null);
            })
            .on("dblclick", function () {
                const allOthers = color.domain().filter(n => n !== name);
                const isAlreadySolo = allOthers.every(n => hidden.has(n));

                if (isAlreadySolo) {
                    // restore all
                    hidden.clear();
                    legend.selectAll(".legend-item").style("opacity", 1);
                } else {
                    // hide everything except this one
                    hidden.clear();
                    allOthers.forEach(n => hidden.add(n));

                    legend.selectAll(".legend-item")
                        .style("opacity", d => d === name ? 1 : 0.3);
                }

                svg.selectAll("circle")
                    .style("display", d => hidden.has(d.name) ? "none" : null);
            });

        // Color box
        item.append("div")
            .style("width", "15px")
            .style("height", "15px")
            .style("background", color(name))
            .style("flex-shrink", "0")
            .style("user-select", "none");   // prevent selection

        // Text label
        item.append("span")
            .text(name)
            .style("user-select", "none");   // prevent selection
    });

    d3.select("#legend-container-hrd")
    .append("div")
    .style("font-family", "Roboto, sans-serif")
    .style("font-size", isMobile ? "11px" : "13px")
    .style("color", "#666")
    .style("padding", isMobile ? "4px 10px 10px 10px" : "4px 25px 10px 100px")
    .text("Click on a region to hide or show it on the plot. Double click a region to isolate it.");



    // intersection observer for scroll-based color changes
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting && window.innerWidth > 991) {
                const scheme = schemes[entry.target.id];
                if (scheme) {
                    svg.selectAll("circle")
                        .transition()
                        .duration(500)
                        .attr("fill", d => scheme(d.name));
                }
            }
        });
    }, { threshold: 0.5 });

    ["hrd-default", "hrd-lac", "hrd-wasia"].forEach(id => {
        const el = document.getElementById(id);
        if (el) observer.observe(el);
    });

});