# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create data frames from the provided CSV data
sub1_data <- data.frame(
  trial = 1:15,
  phase = c(rep("practice", 5), rep("experiment", 10)),
  matrix = c(
    "I R U C\\nJ E X L\\nE B G R", "U R Q P\\nT I H B\\nP H E Y", 
    "V G N T\\nV S E K\\nO U P Z", "G K F I\\nK J T W\\nE S G N", 
    "M I N P\\nX M I V\\nE V J C", "L F C X\\nQ G O I\\nD K P V", 
    "H S O Z\\nI P X T\\nU B R J", "Y T J G\\nG L F D\\nE X V T", 
    "K S I T\\nC Z I F\\nY K X Q", "C B U Y\\nY P Q G\\nL Y E O", 
    "A E P B\\nS I P A\\nS V L P", "W P F I\\nW S E B\\nY O E N", 
    "X H F N\\nS Z O B\\nN U E W", "M F J L\\nP R J K\\nV Q W F", 
    "Y D H Q\\nJ A S X\\nR A X W"
  ),
  tone_row = c(1, 0, 2, 1, 1, 1, 2, 2, 1, 2, 2, 0, 0, 2, 1),
  correct_row = c(
    "JEXL", "URQP", "OUPZ", "KJTW", "XMIV", "QGOI", "UBRJ", "EXVT", 
    "CZIF", "LYEO", "SVLP", "WPFI", "XHFN", "VQWF", "JASX"
  ),
  response_row = c(
    "JXLC", "PHEB", "OUPZ", "KJTW", "XMIV", "QGOL", "UBPRJ", "GLFD", 
    "CZIF", "YQPZ", "AFPB", "WSEB", "SZOB", "GSGG", "JASX"
  )
)

sub2_data <- data.frame(
  trial = 1:15,
  phase = c(rep("practice", 5), rep("experiment", 10)),
  matrix = c(
    "B I H X\\nN K H G\\nL X G R", "K Z Q M\\nC B R T\\nI Y G C", 
    "L Z W H\\nR E T O\\nC W J H", "C K G S\\nG A B F\\nU G M X", 
    "C W J M\\nL D P O\\nT Y I E", "A M Q S\\nE I L G\\nI K H L", 
    "O R V K\\nU J C I\\nA D O B", "B F W X\\nL D J B\\nE R I V", 
    "R L P K\\nZ V I F\\nT J U S", "Z P E L\\nQ V Y G\\nP U Z A", 
    "J N B T\\nQ V E Y\\nC M J A", "M C E S\\nO K S N\\nJ B T V", 
    "F M H S\\nY O Z S\\nM P R O", "V A M N\\nR K U F\\nI R P O", 
    "I E Z H\\nU T O G\\nT Y U E"
  ),
  tone_row = c(2, 2, 0, 1, 2, 0, 0, 2, 2, 2, 0, 2, 2, 1, 2),
  correct_row = c(
    "LXGR", "IYGC", "LZWH", "GABF", "TYIE", "AMQS", "ORVK", "ERIV", 
    "TJUS", "PUZA", "JNBT", "JBTV", "MPRO", "RKUF", "TYUE"
  ),
  response_row = c(
    "BIHX", "IYGH", "RETO", "GABO", "IP", "AEGY", "O", "JD", "TVJ", 
    "QUTZ", "JT", "JDT", "M", "VMRF", "TUFM"
  )
)

sub3_data <- data.frame(
  trial = 1:15,
  phase = c(rep("practice", 5), rep("experiment", 10)),
  matrix = c(
    "D Q L R\\nX F B T\\nQ T W G", "L G R J\\nV D J C\\nG U P X", 
    "I T Z A\\nH S C T\\nF S V H", "Y A U E\\nB J K A\\nJ V P X", 
    "E U H D\\nA J Y L\\nB T M J", "Y W B M\\nJ X U B\\nW U S A", 
    "H J D U\\nY R O U\\nB A D T", "U B S F\\nW N A O\\nI J S D", 
    "C S Z U\\nB I V P\\nO B Z R", "C X W U\\nQ P F E\\nX G M K", 
    "M F I H\\nV R X Z\\nZ M C L", "Z Y E G\\nJ D X H\\nS L J O", 
    "V P B K\\nF U L N\\nV H P E", "F D G T\\nB C G H\\nK P F Q", 
    "W B U D\\nU X Y F\\nL F Q M"
  ),
  tone_row = c(0, 1, 0, 2, 1, 2, 0, 1, 0, 1, 0, 2, 1, 1, 0),
  correct_row = c(
    "DQLR", "VDJC", "ITZA", "JVPX", "AJYL", "WUSA", "HJDU", "WNAO", 
    "CSZU", "QPFE", "MFIH", "SLJO", "FULN", "BCGH", "WBUD"
  ),
  response_row = c(
    "DQLR", "PCGF", "RUPY", "YOUT", "IPUT", "UPIY", "BADT", "XDGH", 
    "BIVP", "QPVD", "VRXD", "JDXH", "FULN", "BCGH", "UXVY"
  )
)

# Add participant identifier
sub1_data$participant <- "Sub1"
sub2_data$participant <- "Sub2"
sub3_data$participant <- "Sub3"

# Combine all data
all_data <- bind_rows(sub1_data, sub2_data, sub3_data)

# Function to calculate accuracy
calculate_accuracy <- function(correct, response) {
  correct_chars <- strsplit(correct, "")[[1]]
  response_chars <- strsplit(response, "")[[1]]
  
  # If response is shorter than correct, pad with NA
  if (length(response_chars) < length(correct_chars)) {
    response_chars <- c(response_chars, rep(NA, length(correct_chars) - length(response_chars)))
  }
  
  # If response is longer, truncate
  if (length(response_chars) > length(correct_chars)) {
    response_chars <- response_chars[1:length(correct_chars)]
  }
  
  # Calculate position-wise accuracy
  pos_accuracy <- mapply(function(c, r) ifelse(!is.na(r) && c == r, 1, 0), 
                         correct_chars, response_chars)
  
  # Return overall accuracy and position-wise accuracy
  list(
    overall = mean(pos_accuracy, na.rm = TRUE),
    pos1 = ifelse(length(pos_accuracy) >= 1, pos_accuracy[1], NA),
    pos2 = ifelse(length(pos_accuracy) >= 2, pos_accuracy[2], NA),
    pos3 = ifelse(length(pos_accuracy) >= 3, pos_accuracy[3], NA),
    pos4 = ifelse(length(pos_accuracy) >= 4, pos_accuracy[4], NA)
  )
}

# Apply accuracy calculation
accuracy_results <- all_data %>%
  rowwise() %>%
  mutate(accuracy = list(calculate_accuracy(correct_row, response_row))) %>%
  mutate(overall_accuracy = accuracy$overall,
         pos1 = accuracy$pos1,
         pos2 = accuracy$pos2,
         pos3 = accuracy$pos3,
         pos4 = accuracy$pos4) %>%
  select(-accuracy)

# Calculate average accuracy by participant and phase
phase_accuracy <- accuracy_results %>%
  group_by(participant, phase) %>%
  summarize(avg_accuracy = mean(overall_accuracy, na.rm = TRUE) * 100, .groups = "drop")

# Calculate position-wise accuracy by participant for experimental trials
position_accuracy_exp <- accuracy_results %>%
  filter(phase == "experiment") %>%
  group_by(participant) %>%
  summarize(
    pos1 = mean(pos1, na.rm = TRUE) * 100,
    pos2 = mean(pos2, na.rm = TRUE) * 100,
    pos3 = mean(pos3, na.rm = TRUE) * 100,
    pos4 = mean(pos4, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Create performance comparison plot (practice vs experiment)
p1 <- ggplot(phase_accuracy, aes(x = phase, y = avg_accuracy, fill = participant)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(avg_accuracy, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Performance Comparison: Practice vs Experimental Trials",
       x = "Phase", y = "Accuracy (%)", fill = "Participant") +
  theme_minimal() +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  ylim(0, 100) +
  theme(legend.position = "bottom")

# Prepare data for position-wise accuracy plot without pivot_longer
position_data <- data.frame(
  participant = rep(position_accuracy_exp$participant, each = 4),
  position = rep(1:4, times = nrow(position_accuracy_exp)),
  accuracy = c(
    position_accuracy_exp$pos1,
    position_accuracy_exp$pos2,
    position_accuracy_exp$pos3,
    position_accuracy_exp$pos4
  )
)

# Create position-wise accuracy plot
p2 <- ggplot(position_data, aes(x = position, y = accuracy, color = participant, group = participant)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = round(accuracy, 1)), vjust = -1, size = 3) +
  labs(title = "Position-wise Accuracy in Experimental Trials",
       x = "Position in Row", y = "Accuracy (%)", color = "Participant") +
  theme_minimal() +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_x_continuous(breaks = 1:4, labels = c("1", "2", "3", "4")) +
  ylim(0, 100) +
  theme(legend.position = "bottom")

# Prepare data for detailed position-wise accuracy plot without pivot_longer
detailed_position_data <- data.frame(
  participant = rep(position_accuracy_exp$participant, each = 4),
  position = factor(rep(1:4, times = nrow(position_accuracy_exp))),
  accuracy = c(
    position_accuracy_exp$pos1,
    position_accuracy_exp$pos2,
    position_accuracy_exp$pos3,
    position_accuracy_exp$pos4
  )
)

# Create a detailed position-wise accuracy plot for each participant
p3 <- ggplot(detailed_position_data, aes(x = position, y = accuracy, fill = participant)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(accuracy, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Detailed Position-wise Accuracy by Participant",
       x = "Position in Row", y = "Accuracy (%)", fill = "Participant") +
  theme_minimal() +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  ylim(0, 100) +
  theme(legend.position = "bottom")

# Save the plots
ggsave("performance_comparison.png", p1, width = 6, height = 5, dpi = 300)
ggsave("position_accuracy.png", p2, width = 6, height = 5, dpi = 300)
ggsave("detailed_position_accuracy.png", p3, width = 8, height = 6, dpi = 300)

# Print the plots
print(p1)
print(p2)
print(p3)
