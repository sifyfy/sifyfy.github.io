(() => {
    function compareDate(a, b) {
        if (a > b) {
            return 1;
        } else if (a < b) {
            return -1;
        } else {
            return 0;
        }
    }

    function reverseCompareResult(r) {
        return r * -1;
    }

    window.Card.loadItems(
        'https://api.github.com/users/sifyfy/repos',
        'main .github .repositories',
        10,
        (a, b) => reverseCompareResult(compareDate(new Date(a.pushed_at), new Date(b.pushed_at))),
        'name',
        'html_url',
        null,
        'description'
    );

    window.Card.loadItems(
        'https://qiita.com/api/v2/users/siphilia_rn/items?page=1&per_page=' + 100,
        'main .qiita .posts',
        100,
        (a, b) => reverseCompareResult(compareDate(new Date(a.created_at), new Date(b.created_at))),
        'title',
        'url',
        'created_at'
    );
})();