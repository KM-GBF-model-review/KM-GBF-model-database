rm(list = ls())

targets::tar_source("R")

database <- LoadDatabaseInR(dataset_path = "input/Merged_Dataset_KMGBF_13Oct25.xlsx") 



validation <- database[,c("QA_Questionnaire_validation.Validated","QA_Questionnaire_validation.Additional_questions")]


validation_freq <- data.frame(name = stringr::str_wrap(c("Validated by modeller/user","Additional questions answered"), 12),
                              frequency = apply(validation, MARGIN = 2, FUN = function(x) sum(x == 1)))

validation_freq$name <- factor(validation_freq$name, levels = stringr::str_wrap(c("Validated by modeller/user","Additional questions answered"), 12))


validation_plot <- ggplot2::ggplot(data = validation_freq, ggplot2::aes(x = name, y = frequency)) +
  ggplot2::geom_bar(stat = "identity", colour = "black", fill = "grey") +
  ggplot2::geom_hline(linetype = "dashed", yintercept = nrow(database), linewidth = 1) +
  ggplot2::ylab("Frequency") +
  ggplot2::theme(#axis.line.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text( size = 20),
                 axis.text.x = ggplot2::element_text( size = 20, face = "bold"),
                 #axis.ticks.y = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text( size = 20, face = "bold"),
                 axis.title.x = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "white", color = "white"),
                 panel.grid = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank())



ggplot2::ggsave(validation_plot,
                filename = "outputs/figures/validation/validation_plot.png",
                device = "png", width = 30.5, height = 30, units = "cm", dpi = 400)
