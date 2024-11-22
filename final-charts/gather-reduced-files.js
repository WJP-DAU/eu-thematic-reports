const path = require('path');
const fsex = require('fs-extra');

// List of directories to copy files from
const sourceDirs = [
    path.join(__dirname, 'no-tooltip/R1'),
    path.join(__dirname, 'no-tooltip/R2'),
    path.join(__dirname, 'no-tooltip/R3')
];

// List of specific files to copy
const filesToCopy = [
    "R1F1.svg", "R1F2.svg", "R1F5.svg", "R1F6.svg", "R1F8.svg", "R1F11.svg", "R1F18.svg", "R1F19.svg", "R1F20.svg",
    "R1F21.svg", "R1F22.svg", "R1F23.svg", "R1F24.svg", "R1F25.svg", "R1F26.svg", "R1F27.svg", "R1F28.svg", "R1F29.svg",
    "R1F30.svg", "R1F31.svg", "R1F32.svg", "R1F33.svg", "R1F34.svg", "R1F35.svg", "R1F36.svg", "R2F25.svg", "R2F26.svg",
    "R2F27.svg", "R2F28.svg", "R2F29.svg", "R3F19.svg"
];

// Destination directory where the files will be copied to
const destDir = path.join(__dirname, 'reduced');

// Function to copy specific files from multiple directories to the destination
async function copyFiles() {
    try {
        // Create destination directory if it doesn't exist
        await fsex.ensureDir(destDir);

        // Loop through each source directory
        for (const sourceDir of sourceDirs) {
            for (const fileName of filesToCopy) {
                const sourceFilePath = path.join(sourceDir, fileName);
                const destFilePath = path.join(destDir, fileName);

                // Check if the file exists in the source directory
                try {
                    await fsex.access(sourceFilePath); // Check if the file exists
                    await fsex.copy(sourceFilePath, destFilePath); // Copy the file
                    console.log(`Copied ${fileName} to ${destDir}`);
                } catch (err) {
                    console.log(`File ${fileName} not found in ${sourceDir}`);
                }
            }
        }
    } catch (err) {
        console.error('Error copying files:', err);
    }
}

// Run the copy process
copyFiles();

