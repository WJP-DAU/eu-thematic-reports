const fs = require('fs');
const path = require('path');
const { optimize } = require('svgo');

// Directory containing SVG files
const svgDir = path.join(__dirname, 'reduced');
// const svgDir = path.join(__dirname, 'no-tooltip/R1');
// const svgDir = path.join(__dirname, 'no-tooltip/R2');
// const svgDir = path.join(__dirname, 'no-tooltip/R3');

// Function to optimize SVG files
const optimizeSVGs = (directory) => {
    // Read all files in the directory
    fs.readdir(directory, (err, files) => {
      if (err) {
        console.error('Error reading directory:', err);
        return;
      }
  
      // Filter for only .svg files
      const svgFiles = files.filter((file) => path.extname(file).toLowerCase() === '.svg');
  
      // Process each SVG file
      svgFiles.forEach((file) => {
        const filePath = path.join(directory, file);
  
        // Read the SVG file
        fs.readFile(filePath, 'utf8', (err, data) => {
          if (err) {
            console.error(`Error reading file ${file}:`, err);
            return;
          }
  
          // Optimize the SVG content
          const result = optimize(data, {
            path: filePath, // Optional: Pass the file path to SVGO
          });
  
          // Write the optimized SVG back to the same file
          fs.writeFile(filePath, result.data, (err) => {
            if (err) {
              console.error(`Error writing file ${file}:`, err);
              return;
            }
            console.log(`Optimized: ${file}`);
          });
        });
      });
    });
  };
  
// Call the function
optimizeSVGs(svgDir);