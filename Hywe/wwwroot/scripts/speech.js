window.startTranscription = () => {
    return new Promise((resolve) => {
        const SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
        if (!SpeechRecognition) {
            alert("Speech recognition is not supported.");
            resolve();
            return;
        }

        const recognition = new SpeechRecognition();
        const textArea = document.getElementById("hynteract-desc-input");

        recognition.onresult = (event) => {
            const transcript = event.results[0][0].transcript;
            const currentVal = textArea.value;
            textArea.value = currentVal ? `${currentVal} ${transcript}` : transcript;
            textArea.dispatchEvent(new Event('input', { bubbles: true }));
        };

        // This is the key: the F# 'await' finishes when this resolves
        recognition.onend = () => resolve();
        recognition.onerror = () => resolve();

        recognition.start();
    });
};