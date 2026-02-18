window.startTranscription = () => {
    const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
    if (!SpeechRecognition) {
        alert("Speech recognition is not supported in this browser.");
        return;
    }

    const recognition = new SpeechRecognition();
    const textArea = document.getElementById("hynteract-desc-input");

    recognition.onresult = (event) => {
        const transcript = event.results[0][0].transcript;

        // Append transcription to current text
        const currentVal = textArea.value;
        textArea.value = currentVal ? `${currentVal} ${transcript}` : transcript;

        // CRITICAL: Trigger 'input' event so Bolero's 'on.input' captures the change
        textArea.dispatchEvent(new Event('input', { bubbles: true }));
    };

    recognition.start();
};