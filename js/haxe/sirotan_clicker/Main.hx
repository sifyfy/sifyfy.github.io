import js.JQuery;
import js.html.Element;

class Main
{
    static public var w:Int;

    static function main()
    {
        w = 50;
        var element = new JQuery("#SirotanClicker img");
        element.click(
            function(e)
            {
                ++w;
                element.get()[0].setAttribute("width", '${w}px');
                return;
            }
        ).mouseover(
            function(e){ element.addClass("on"); return; }
        ).mouseleave(
            function(e){ element.removeClass("on"); return; }
        );
    }
}