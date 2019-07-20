(() => {
    window.Card.loadItems(
        'https://api.github.com/users/sifyfy/repos',
        'main .github .repositories',
        10,
        (a, b) => new Date(a.pushed_at) <= new Date(b.pushed_at),
        'name',
        'html_url',
        null,
        'description'
    );

    window.Card.loadItems(
        'https://qiita.com/api/v2/users/siphilia_rn/items?page=1&per_page=' + 100,
        'main .qiita .posts',
        100,
        (a, b) => new Date(a.created_at) <= new Date(b.created_at),
        'title',
        'url',
        'created_at'
    );
})();