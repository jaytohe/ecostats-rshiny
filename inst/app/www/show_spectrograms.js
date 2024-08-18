$( document ).ready(function() {
  // Click event listener for the show spectrograms button

  $("#show_spectrograms").on("click", function() {
    if (typeof checkboxState !== "undefined") {

      //Create array of [<row_id>, <associated spectrogram image>]
      const checkedImages = Object.entries(checkboxState)
      .filter(state => state[1])//filter by checked : true
      .map(state => state[0]) // extract only row ids
      .map(id => [parseInt(id), getSpectroImageByRow(id)]);

        console.log(checkedImages);
      // Generate new HTML content
            var newWindowContent = `
            <!DOCTYPE html>
            <html lang="en">
            <head>
                <meta charset="UTF-8">
                <title>Selected Images</title>
                <style>
                    body {
                        transform-origin: top left;
                        transition: transform 0.2s;
                    }
                    .image-grid {
                        display: grid;
                        grid-template-columns: 1fr 1fr;
                        gap: 10px;
                    }
                    .image-grid img {
                        width: 100%;
                        height: 300px;
                        object-fit: cover;
                    }
                </style>
            </head>
            <body>
                <button onclick="zoomIn()">Zoom In</button>
                <button onclick="zoomOut()">Zoom Out</button>
                <div class="image-grid">
                    ${checkedImages.map(j => `<div><figure><img src="${j[1]}" alt="Image"><figcaption>Row ID: ${j[0]+1}</figcaption></div>`).join('')}
                </div>
                <script>
                    let scale = 1;
                    function zoomIn() {
                        scale += 0.1;
                        document.body.style.transform = 'scale(' + scale + ')';
                    }
                    function zoomOut() {
                        scale -= 0.1;
                        document.body.style.transform = 'scale(' + scale + ')';
                    }
                <\/script>
            </body>
            </html>
            `;

            // Open new tab and write HTML content
            var newWindow = window.open();
            newWindow.document.write(newWindowContent);
            newWindow.document.close();

    }
  });
});
