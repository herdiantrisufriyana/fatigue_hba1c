# Large language model-assisted causal machine learning for identifying fatigue-related poor glycated hemoglobin in type 2 diabetes

Herdiantri Sufriyana,1,¶ Debby Syahru Romadlon,2,¶ Rudy Kurniawan,3 Safiruddin Al Baqi,4 Emmanuel Ekpor5, Eric Peprah Osei6, Hsiao-Yean Chiu, 7,8,9,10,* Emily Chia-Yu Su.1,11,12*

1 Institute of Biomedical Informatics, College of Medicine, National Yang Ming Chiao Tung University, Taipei, Taiwan.
2 Faculty of Nursing, Chulalongkorn University, Bangkok, Thailand.
3 Diabetes Connection Care, Eka Hospital Bumi Serpong Damai, Tangerang, Indonesia.
4 Faculty of Education and Teaching Sciences, State Islamic Institute of Ponorogo, Ponorogo, Indonesia.
5 School of Nursing, University of Ghana, Legon, Ghana.
6 College of Nursing, University of Illinois Chicago, Illinois, United States.
7 School of Nursing, College of Nursing, Taipei Medical University, Taipei, Taiwan.
8 Research Center of Sleep Medicine, College of Medicine, Taipei Medical University, Taipei, Taiwan.
9 Department of Nursing, Taipei Medical University Hospital, Taipei, Taiwan.
10 Research Center of Sleep Medicine, Taipei Medical University Hospital, Taipei, Taiwan.
11 Graduate Institute of Biomedical Informatics, College of Medical Science and Technology, Taipei Medical University, Taipei, Taiwan.
12 Clinical Big Data Research Center, Taipei Medical University Hospital, Taipei, Taiwan.

¶ Equal contribution as the first authors
* Corresponding authors:
Emily Chia-Yu Su. Institute of Biomedical Informatics, College of Medicine, National Yang Ming Chiao Tung University, 155, Section 2, Linong Street, Beitou District, Taipei 112304, Taiwan. Tel: +886-2-28267000 Ext. 67391. E-mail: emilysu@nycu.edu.tw.
Hsiao-Yean Chiu. School of Nursing, College of Nursing, Taipei Medical University, 250, Wuxing Street, Xinyi District, Taipei 11031, Taiwan. Tel: +886-2-27361661 Ext. 6339. Email address: hychiu0315@tmu.edu.tw.

## Vignette

Explore the vignette [**here**](https://herdiantrisufriyana.github.io/fatigue_hba1c/index.html).

## System requirements

Install Docker desktop once in your machine. Start the service every time you build this project image or run the container.

## Installation guide

Build the project image once for a new machine (currently support AMD64 and ARM64).

```{bash}
docker build -t fatigue_hba1c --load .
```

Run the container every time you start working on the project. Change left-side port numbers for either Rstudio or Jupyter lab if any of them is already used by other applications.

In terminal:

```{bash}
docker run -d -p 8787:8787 -p 8888:8888 -v "$(pwd)":/home/rstudio/project --name fatigue_hba1c_container fatigue_hba1c
```

In command prompt:

```{bash}
docker run -d -p 8787:8787 -p 8888:8888 -v "%cd%":/home/rstudio/project --name fatigue_hba1c_container fatigue_hba1c
```

## Instructions for use

### Rstudio

Change port number in the link, accordingly, if it is already used by other applications.

Visit http://localhost:8787.
Username: rstudio
Password: 1234

Your working directory is ~/project.

### Jupyter lab

Use terminal/command prompt to run the container terminal.

```{bash}
docker exec -it fatigue_hba1c_container bash
```

In the container terminal, run jupyter lab using this line of codes.

```{bash}
jupyter-lab --ip=0.0.0.0 --no-browser --allow-root
```

Click a link in the results to open jupyter lab in a browser. Change port number in the link, accordingly, if it is already used by other applications.






