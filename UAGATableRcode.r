library(stringr)
library(gridExtra)
library(grid)
library(gtable)

# Read and prepare data
matrix_data <- read.csv("UAGA_state_laws.csv", stringsAsFactors = FALSE)

# Create exact column names to match your CSV
column_names <- c(
   "State",
   "Inclusive procurement organization definition", 
   "Required to cooperate",
   "Required to enter agreement",
   "Required to be present in order to deny",
   "Required to examine tissues quickly enough to maintain preservation",
   "Choice of law provision",
   "Good faith immunity clause"
)

# Ensure we're working with the right columns
matrix_data <- matrix_data[, column_names, drop = FALSE]

# Data cleaning
matrix_data[matrix_data == "[blank]"] <- ""
matrix_data[is.na(matrix_data)] <- ""


# Define more compact headers
new_headers <- c(
   "",
   "Inclusive Definition",
   "Cooperation Req.",
   "Agreement Req.",
   "Present To Deny", 
   "Timely Manner Req.",
   "Choice of Law",
   "Good Faith Immunity"
)

colnames(matrix_data) <- new_headers


# Create table with 45-degree rotated headers
table <- tableGrob(matrix_data,
   rows = NULL,
   theme = ttheme_minimal(
       core = list(
           fg_params = list(
               cex = 0.7,
               fontfamily = "sans",
               hjust = 0,
               x = 0.1
           ),
           bg_params = list(
               fill = "white",
               col = NA,
               lwd = 0
           ),
           padding = unit(c(0.2, 0.2), "cm")
       ),
       colhead = list(
           fg_params = list(
               cex = 0.7,
               fontfamily = "sans",
               just = "left",
               rot = 45
           ),
           bg_params = list(
               fill = "white",
               col = NA,
               lwd = 0
           ),
           padding = unit(c(0, 0), "cm")
       )
   )
)

# Add horizontal lines to every row
table <- gtable::gtable_add_grob(table,
   grobs = replicate(nrow(matrix_data) - 1,
       segmentsGrob(
           x0 = unit(0, "npc"),
           x1 = unit(1, "npc"),
           y0 = unit(0, "npc"),
           y1 = unit(0, "npc"),
           gp = gpar(col = "gray60", alpha = 0.5)
       ),
       simplify = FALSE
   ),
   t = 2:(nrow(matrix_data)),
   l = 1,
   r = ncol(table)
)

# Add top border line (between header and first state)
table <- gtable::gtable_add_grob(table,
   grob = segmentsGrob(
       x0 = unit(0, "npc"),
       x1 = unit(1, "npc"),
       y0 = unit(1, "npc"),
       y1 = unit(1, "npc"),
       gp = gpar(col = "gray60", alpha = 0.5)
   ),
   t = 2,
   l = 1,
   r = ncol(table)
)

# Add line at bottom of header
table <- gtable::gtable_add_grob(table,
   grob = segmentsGrob(
       x0 = unit(0, "npc"),
       x1 = unit(1, "npc"),
       y0 = unit(0, "npc"),
       y1 = unit(0, "npc"),
       gp = gpar(col = "gray60", alpha = 0.5)
   ),
   t = 1,
   l = 1,
   r = ncol(table)
)

# Add borders
for(pos in c(2, nrow(table))) {
   table <- gtable::gtable_add_grob(table,
       grob = segmentsGrob(
           x0 = unit(0, "npc"),
           x1 = unit(1, "npc"),
           y0 = unit(0, "npc"),
           y1 = unit(0, "npc"),
           gp = gpar(col = "gray60", alpha = 0.5)
       ),
       t = pos,
       l = 1,
       r = ncol(table)
   )
}

# Adjust dimensions
table$heights[1] <- unit(2.75, "cm")
table$widths[1] <- unit(2.75, "cm")      
table$widths[2:8] <- unit(2.75, "cm") 

# Output to PDF
pdf("UAGA_State_Laws.png",
   width = 10,
   height = 10,
   useDingbats = FALSE
)
grid.draw(table)
dev.off()