d3.csv("https://raw.githubusercontent.com/biancaschutz/hroutlook/refs/heads/main/data/life_expectancy_beeswarm.csv", function (data) {
    // Data preparation 
    data.forEach(d => {
        d.FactValueNumeric = +d.FactValueNumeric;
    });

    const region = d3.nest()
        .key(d => d["Region Name"])
        .entries(data);

    region.forEach(l => {
        l.values.forEach(v => v["Region Name"] = l.key);
    });

    const names = [...new Set(data.map(d => d.Location))];

    const values = data.map(d => +d.FactValueNumeric);

    const regionNames = region.map(d => d.key).sort();

    console.log(regionNames);

    const isMobile = window.innerWidth <= 991;

    const width = isMobile ? window.innerWidth - 20 : 800;
    const marginRight = 20;
    const marginLeft = isMobile ? 60 : 100;
    const marginBottom = 20;
    const marginTop = 40;
    const height = regionNames.length * 200;

    const radius = isMobile ? 3.5 : 5;
    const padding = isMobile ? 1.5 : 2.5;

    // functions for tooltip 
    var mousemove = function (d) {
        const ttWidth = tooltip.node().offsetWidth;

        tooltip
            .html(`In 2021, ${d.data.Location} had a life expectancy of ${d.data.FactValueNumeric.toFixed(0)} years at birth.`)

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
        pinnedTooltip = false;
        // reset any enlarged dot
        svg.selectAll("circle")
            .transition().duration(200)
            .attr("r", radius);
        tooltip.transition()
            .duration(0)
            .style("opacity", 1);
    };

    var mouseleave = function (d) {
        if (!pinnedTooltip) {
            tooltip.transition()
                .duration(500)
                .style("opacity", 0);
        }
    };

    const x = d3.scaleLinear()
        .domain([d3.min(values) - 5, d3.max(values)])
        .range([marginLeft, width - marginRight]);

    const y = d3.scalePoint()
        .domain(region.map(d => d.key).sort())
        .rangeRound([marginTop, height - marginBottom])
        .padding(.4);

    const color = d3.scaleOrdinal()
        .domain(['Africa', 'Americas', 'Asia', 'Europe', 'Oceania'])
        .range(['#81ac31', '#16b5c5', '#f78e1e', '#b95380', '#8a69a1']);

    const schemes = {
        "lifeexp-default": d3.scaleOrdinal()
            .domain(['Africa', 'Americas', 'Asia', 'Europe', 'Oceania'])
            .range(['#81ac31', '#16b5c5', '#f78e1e', '#b95380', '#8a69a1']),

        "lifeexp-africa": d3.scaleOrdinal()
            .domain(['Africa', 'Americas', 'Asia', 'Europe', 'Oceania'])
            .range(['#81ac31', '#ccc', '#ccc', '#ccc', '#ccc']),

        "lifeexp-higher": d3.scaleOrdinal()
            .domain(['Africa', 'Americas', 'Asia', 'Europe', 'Oceania'])
            .range(['#ccc', '#16b5c5', '#f78e1e', '#b95380', '#ccc']),

        "lifeexp-europe": d3.scaleOrdinal()
            .domain(['Africa', 'Americas', 'Asia', 'Europe', 'Oceania'])
            .range(['#ccc', '#ccc', '#ccc', '#b95380', '#ccc']),
    };

const naturalHeight = regionNames.length * 200;

const svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", naturalHeight)
    .attr("viewBox", [0, 0, width, naturalHeight])
    .attr("preserveAspectRatio", "xMidYMid meet")
    .style("width", "100%")
    .style("height", "100%")
    .style("display", "block");

    // adding the x-axis
    svg.append("g")
        .attr("transform", `translate(0,${marginTop})`)
        .call(d3.axisTop(x).ticks(null))
        .call(g => g.selectAll("text")
            .style("font-family", "Roboto")
            .style("font-size", isMobile ? "11px" : "20px")
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

    // adding the y-axis
    const g = svg.append("g")
        .attr("text-anchor", "end")
        .style("font-family", "Roboto")
        .style("font-size", isMobile ? "11px" : "20px")
        .selectAll()
        .data(region)
        .enter().append("g")
        .attr("transform", d => `translate(0,${y(d.key)})`);

    g.append("foreignObject")
        .attr("x", marginLeft - (isMobile ? 55 : 160))
        .attr("y", -8)
        .attr("width", isMobile ? 50 : 140)
        .attr("height", 40)
        .append("xhtml:div")
        .style("font-family", "Roboto")
        .style("font-size", isMobile ? "11px" : "20px")
        .style("text-align", "right")
        .style("word-wrap", "break-word")
        .text(d => d.key);

    // adding the dots
    svg.append("g")
        .selectAll("circle")
        .data(dodge(data, { radius: radius * 2 + padding, x: d => x(d.FactValueNumeric) }))
        .enter().append("circle")
        .attr("cx", d => d.x)
        .attr("cy", d => y(d.data["Region Name"]) - d.y)
        .attr("r", radius)
        .attr("fill", d => color(d.data["Region Name"]))
        .on("mouseover", mouseover)
        .on("mousemove", mousemove)
        .on("mouseleave", mouseleave);

    // adding tooltip 
    var tooltip = d3.select("#chart-container-lifeexp").append("div")
        .attr("class", "lifeexp-tooltip")
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


    document.getElementById("chart-container-lifeexp").appendChild(svg.node());


    // intersection observer for scroll-based color changes
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting && window.innerWidth > 991) {
                const scheme = schemes[entry.target.id];
                if (scheme) {
                    svg.selectAll("circle")
                        .transition()
                        .duration(500)
                        .attr("fill", d => scheme(d.data["Region Name"]));
                }
            }
        });
    }, { threshold: 0.5 });

    ["lifeexp-default", "lifeexp-africa", "lifeexp-higher", "lifeexp-europe"].forEach(id => {
        const el = document.getElementById(id);
        if (el) observer.observe(el);
    });
});
