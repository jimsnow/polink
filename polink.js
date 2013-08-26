/* 
 * Javascript glue for polink.org to query reputation server
 * and update the dom with rendered reputation values.
 */


// Reputation server URL
var RSURLBASE = "http://polink.org:9000/rss/polink";

// Constants for rendering a reputation value.
var RR_W  = 60;
var RR_H = 12;
var RR_SCALE  = 7;
var RR_BASE  = 3;

var SVGNS = "http://www.w3.org/2000/svg";

function newsvg(tag) {
  return document.createElementNS(SVGNS, tag);
}

function logbase(n, base) {
    return (Math.log(n)) / (Math.log(base));
}

// Make drawing a rectangle a little more declarative.
function rectangle(x1, y1, x2, y2, style) {
  var r = newsvg("rect");
  r.setAttribute("x", x1);
  r.setAttribute("y", y1);
  r.setAttribute("width", x2-x1);
  r.setAttribute("height", y2-y1);
  r.setAttribute("class", style);

  return r;
}

var testRepval = {
  trust: 20,
  distrust: 10.0,
  gullible: 10.0
};

function rep_scale(x) {
  //return logbase(x+RR_BASE, RR_BASE) * RR_SCALE;
  return Math.pow(x, 0.4)*5;
}

// Generate an svg object from a reputation value.
function render_repval(repval) {
  var t = rep_scale(repval.trust);
  var d = rep_scale(repval.distrust);
  var g = rep_scale(repval.gullible);
  var xmid = RR_W/2;
  var ymid = RR_H/2;
  var ywidth = 3;

  var svg = newsvg("svg");

  //svg.setAttribute("title", "placeholder-title");
  //svg.setAttribute("desc", "placeholder-description");

  svg.setAttribute("width", RR_W);
  svg.setAttribute("height", RR_H);
 
  svg.setAttribute("role", "img");
  svg.setAttribute("aria-label", "placeholder text");

  var title = newsvg("title");
  title.textContent = "pos: " + repval.trust.toFixed(3) +
                      " neg: " + repval.distrust.toFixed(3) +
                      " neg-assoc: " + repval.gullible.toFixed(3);

  var group = newsvg("g");
  svg.appendChild(group);

  group.appendChild(title);

  group.appendChild(rectangle(1, 1, xmid-1, RR_H-1, "rep_frame"));
  group.appendChild(rectangle(xmid+1, 1, RR_W-1, RR_H-1, "rep_frame"));

  group.appendChild(rectangle(xmid-(t+1), ymid-ywidth, xmid-1, ymid+ywidth, "rep_t"));
  group.appendChild(rectangle(xmid+1, ymid, xmid+d+1, ymid+ywidth+1, "rep_d"));  
  group.appendChild(rectangle(xmid+1, ymid-(ywidth+1), xmid+g+1, ymid, "rep_g"));

  group.appendChild(rectangle(xmid-1, 0, xmid+1, RR_H, "rep_divider"));


  return svg;
}

$(document).ready(function() {
  var userid;
  var username;
  var rsurl;
  var pov;
  var doreps = true;
  var entity;
  var ename;
  var povn;

  // Figure out who we're logged in as.
  $('#username').each(function () {
    userid = "u" + this.getAttribute("uid");
    username = this.getAttribute("uname");

    console.log(userid + " " + username);
  });

  $('#thisentity').each(function () {
    entity = "e" + this.getAttribute("eid");
    ename = this.getAttribute("ename");

    console.log(entity + " " + ename);
  });

  // Query the reputation server for reputation values of all
  // the entities on the page, insert svg icon next to each
  // entity.
  
  pov = $.cookie("polink-pov");
  if (pov == null) {
    console.log("no pov");
    pov = "neutral";
  }

  povn = $.cookie("polink-povname");
  if (povn == null) {
    console.log("no povname");
    povn = "neutral";
  }

  console.log("pov: " + pov + " name:" + povn);

  switch (pov.charAt(0)) {
    case 'x':
      rsurl = "";
      doreps = false;
      break;
    case 'n':
      rsurl = "/ratings/";
      break;
    case 'l':
    case 'd':
      rsurl = "/groups/" + pov + "/ratings/";
      break;
    case 'e':
      rsurl = "/users/" + pov + "/ratings/";
      break;
    default:
      rsurl = "/ratings/";
  }

  console.log("rsurl: " + rsurl);

  if (doreps) {
    $('.entity').each(function () {
      var eid = "e" + this.id;
      var span = this.parentNode;

      var url = RSURLBASE + rsurl + eid;

      //console.log("fetching " + url);

      $.getJSON(url, function(repval){
        span.appendChild(render_repval(repval));
      });
    });
  }

  var selstring = "selected=\"selected\" ";

  var nonesel = (pov == "x")            ? selstring : "";
  var neutralsel = (pov == "neutral")      ? selstring : "";
  var thisentsel = (pov == entity)      ? selstring : "";
  //var otherentsel = (pov.charAt(0) == 'e' && pov != entity) ? selstring : "";
  var ulikesel = (pov == 'likedby-' + userid)    ? selstring : "";
  var udislikesel = (pov == 'dislikedby-' + userid) ? selstring : "";

  var loggedinopts =
    (userid == null)
      ? ""
      : "<option value=\"self_l\" " + ulikesel + ">liked by me</option> \
         <option value=\"self_d\" " + udislikesel + ">disliked by me</option>";

  var otherlikeopts =
    (pov.charAt(0) == 'l' && pov != "likedby-" + userid)
      ? "<option " + selstring + ">" + povn + "</option>"
      : "";

  var otherdislikeopts =
    (pov.charAt(0) == 'd' && pov != "dislikedby-" + userid)
      ? "<option " + selstring + ">" + povn + "</option>"
      : "";

  var entopt =
    (entity == null)
      ? ""
      : "<option value=\"entity\" " + thisentsel + ">" + (ename).toLowerCase() + "</option>";

  var otherentopt =
    (pov.charAt(0) == 'e' && pov != entity)
      ? "<option " + selstring + ">" + povn + "</option>"
      : "";

  $('.footer > center').append(
        "| point of view \
         <select id=\"povselector\"> \
           <option value=\"none\" " + nonesel + ">none</option> \
           <option value=\"neutral\" "+ neutralsel + " >neutral</option>" +
           loggedinopts +
           entopt +
           otherlikeopts +
           otherdislikeopts +
           otherentopt +
        "</select>"
      );

  $('#povselector').change(function(){
    var val = $(this).val();
    var povcookie;

    switch (val) {
      case "self_l":
        povcookie = "likedby-" + userid;
        povname = "liked by " + username;
        break;
      case "self_d":
        povcookie = "dislikedby-" + userid;
        povname = "disliked by " + username;
        break;
      case "entity":
        povcookie = entity;
        povname = ename;
        break;
      case "none":
        povcookie = "x";
        povname = "none";
        break;
      default:
        povcookie = val;
        povname = val;
    }

    $.cookie("polink-pov", povcookie, {expires: 90, path: '/'});
    $.cookie("polink-povname", povname, {expires: 90, path: '/'});

    console.log("setting cookies " + povcookie + " " + povname);
    window.location.reload();
  });

  //$('.footer').each(function () {
  //  this.appendChild(document.createTextNode("<p>hello world</p>"));
  //});

  //document.body.appendChild(povSelector);

    //var target = document.getElementById("foo");
    //var parent = target.parentNode;

    //parent.replaceChild(render_repval(testRepval), target);


    //target.appendChild(render_repval(testRepval));

    // document.body.appendChild(svg);


    //$( target ).html( "<p>foobar</p>" );
});
