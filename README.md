# Naive Text Generator

I created this project to dip my toes into text generation. Even though this project uses a naive method, checking word n-tuple frequencies to predict the next word, it creates surprisingly natural outputs.

The core idea is that you provide it a short phrase and it will complete it for you. You can adjust elements like the temperature which will lower the threshold for acceptable next words and make the program more "creative". Feel free to play around with this!

## How to Use

To use the Naive Text Generator, follow these steps:

1. **Install SML/NJ**  
    Make sure you have [Standard ML of New Jersey (SML/NJ)](https://www.smlnj.org/) installed on your system.

2. **Clone the Repository**  

    ```sh
    git clone https://github.com/yourusername/naive-text-generator.git
    cd naive-text-generator
    ```

3. **Run the Program**  
    Start the SML interpreter and load the `main.sml` file:

    ```sh
    sml < main.sml
    ```

    This will compile and run the program.

4. **Interact with the Generator**  
    Follow the on-screen prompts to enter a starting phrase. The program will then generate text based on your input.

Feel free to experiment with different phrases and settings!

## License

This project is for educational and experimental purposes.
