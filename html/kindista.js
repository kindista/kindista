function KsetDisplay(Kelement, Kvalue) {
    return Kelement.style.display = Kvalue;
};
function KsubmitImageForm() {
    KsetDisplay(document.getElementById('spinner'), 'inline-block');
    return document.forms.imageform.submit();
};
