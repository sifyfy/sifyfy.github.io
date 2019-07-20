(function() {
    if (window.Card) {
        return;
    }

    class Card {
        async loadItems(url, destSelector, n, sorter, nameKey, urlKey, dateKey, textKey) {
            const r = await fetch(url);
            if (r.ok) {
                const items = await r.json();
                items.sort(sorter);
                const frame = document.querySelector(destSelector);
                for (const item of items.slice(0, n)) {
                    const card = this.makeCard(item[nameKey], item[urlKey], item[dateKey], item[textKey]);
                    frame.appendChild(card);
                }
            }
        }

        makeTitle(title, url) {
            const a = document.createElement('a');
            a.setAttribute('href', url);
            a.appendChild(document.createTextNode(title));
            const div = document.createElement('div');
            div.classList.add('title');
            div.appendChild(a);
            return div;
        }

        makeDate(date) {
            const icon = document.createElement('i');
            icon.classList.add('far', 'fa-calendar-alt');
            const div = document.createElement('div');
            div.classList.add('date');
            div.appendChild(icon);
            div.appendChild(document.createTextNode(` ${date.getFullYear()}/${date.getMonth() + 1}/${date.getDate()}`));
            return div;
        }

        makeText(text) {
            const div = document.createElement('div');
            div.classList.add('text');
            div.appendChild(document.createTextNode(text));
            return div;
        }

        makeCard(title, url, date, text) {
            const titleNode = this.makeTitle(title, url);
            const card = document.createElement('div');
            card.classList.add('card');
            card.appendChild(titleNode);

            if (date) {
                const dateNode = this.makeDate(new Date(date));
                card.appendChild(dateNode);
            }

            if (text) {
                const textNode = this.makeText(text);
                card.appendChild(textNode);
            }

            return card;
        }
    }

    window.Card = new Card();
})();