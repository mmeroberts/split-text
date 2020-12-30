<script src="https://code.jquery.com/jquery-3.5.0.js"></script>
<script>
/* Set the width of the side navigation to 250px and the left margin of the page content to 250px */
function openNav() {
  document.getElementById("mySidenav").style.width = "250px";
  document.getElementById("main").style.marginLeft = "250px";
}

/* Set the width of the side navigation to 0 and the left margin of the page content to 0 */
function closeNav() {
  document.getElementById("mySidenav").style.width = "0";
  document.getElementById("main").style.marginLeft = "0";
}

function hideEnglish() {
$('.p-english').hide()
$('.h1-english').hide()
$('.h3-english').hide()
$('.h5-english').hide()
$('.vq-english').hide()
$('.hideTextEng').hide()
$('.showTextEng').show()
}

function showEnglish() {
$('.p-english').show()
$('.h1-english').show()
$('.h2-english').show()
$('.h3-english').show()
$('.h5-english').show()
$('.vq-english').show()
$('.showTextEng').hide()
$('.hideTextEng').show()
}

function hideBo() {
$('.p-bo').hide()
$('.h1-bo').hide()
$('.h2-bo').hide()
$('.h3-bo').hide()
$('.h5-bo').hide()
$('.vq-bo').hide()
$('.showTextBo').show()
$('.hideTextBo').hide()
}

function showBo() {
$('.p-bo').show()
$('.h1-bo').show()
$('.h2-bo').show()
$('.h3-bo').show()
$('.h5-bo').show()
$('.vq-bo').show()
$('.showTextBo').hide()
$('.hideTextBo').show()
}
</script>
