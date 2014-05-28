function KsetDisplay(Kelement, Kvalue) {
    return Kelement.style.display = Kvalue;
};
function KsubmitImageForm() {
    KsetDisplay(document.getElementById('spinner'), 'inline-block');
    return document.forms.imageform.submit();
};
function KlimitCharacters(Kmessage, KcharCount, Kindicator) {
    Kchars = Kmessage.value.length;
    console.log(document.getElementById(Kindicator).innerHTML);
    console.log(document.getElementById(Kindicator));
    document.getElementById(Kindicator).innerHTML = KcharCount - Kchars;
    return Kchars > KcharCount ? (Kmessage.value = Kmessage.value.substring(0, KcharCount)) : null;
};
