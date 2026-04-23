function dodge(data, { radius, x }) {
    const radius2 = radius ** 2;
    const circles = data.map(d => ({ x: x(d), data: d })).sort((a, b) => a.x - b.x);
    const epsilon = 1e-3;
    let head = null, tail = null;

    function intersects(x, y) {
        let a = head;
        while (a) {
            if (radius2 - epsilon > (a.x - x) ** 2 + (a.y - y) ** 2) return true;
            a = a.next;
        }
        return false;
    }

    for (const b of circles) {
        while (head && head.x < b.x - radius2) head = head.next;
        if (intersects(b.x, b.y = 0)) {
            let a = head;
            b.y = Infinity;
            do {
                let y1 = a.y + Math.sqrt(radius2 - (a.x - b.x) ** 2);
                let y2 = a.y - Math.sqrt(radius2 - (a.x - b.x) ** 2);
                if (Math.abs(y1) < Math.abs(b.y) && !intersects(b.x, y1)) b.y = y1;
                if (Math.abs(y2) < Math.abs(b.y) && !intersects(b.x, y2)) b.y = y2;
                a = a.next;
            } while (a);
        }
        b.next = null;
        if (head === null) head = tail = b;
        else tail = tail.next = b;
    }

    return circles;
}