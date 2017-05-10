
function Logout(pagetype) {
  var txt;
  if (pagetype == "newEntry") {
    txt = "Are you sure you want to logout? Any unsaved progress may be lost.";
  } else {
    txt = "Are you sure you want to logout?"
  }

  if (confirm(txt) == true) {
    sessionStorage.clear();
    window.location="index.html";
  }
}
