# BMHICML: Bone Marrow Histopathology Image Classification via Machine Learning

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![R-4.3.0](https://img.shields.io/badge/R-%E2%89%A54.3.0-%23276DC3.svg?style=flat&logo=r&logoColor=white)](https://cran.r-project.org/bin/windows/base/old/4.3.0/) [![Python 3.9+](https://img.shields.io/badge/python-3.9+-blue.svg)](https://www.python.org/downloads/) [![Nextflow](https://img.shields.io/badge/nextflow-%3E%2023.10-099acd.svg)](https://www.nextflow.io) [![Snakemake](https://img.shields.io/badge/snakemake-%E2%89%A57.32-brightgreen.svg)](https://snakemake.readthedocs.io) [![PyTorch Version](https://img.shields.io/badge/pytorch-1.12%2B-orange)](https://pytorch.org/)

**Repository**: <https://github.com/JhuangLab/BMHICML>

**Principal Investigator**: Jhuanglab

**Contact**: hiekeen $$at$$ gmail.com

### The source code will be made available following the manuscript's publication.

## Project Overview

BMHICML (Bone Marrow Histopathology Image Classification via Machine Learning) is an open-source project dedicated to developing and benchmarking machine learning models for automatic classification of bone marrow histopathology images. It addresses the clinical challenge of labor-intensive manual diagnosis by providing efficient, scalable, and interpretable algorithms that assist pathologists in identifying bone marrow disorders.

The project integrates a curated dataset, state-of-the-art classification models, and evaluation toolkits to support both clinical research and algorithm innovation in hematopathology.

### Key Features

- Multi-Category Classification: Supports classification of common bone marrow disorders (e.g., acute myeloid leukemia, myelodysplastic syndrome, normal marrow) with high granularity.

- Model Zoo: Implements classic and cutting-edge ML/DL models, including CNN-based (ResNet, DenseNet, EfficientNet), transformer-based (ViT, Swin Transformer), and traditional ML (SVM, Random Forest) for comparison.

- Data Preprocessing Pipeline: Provides built-in functions for image normalization, augmentation (rotation, flipping, zooming), patch extraction, and stain normalization to handle histopathology image variability.

- Comprehensive Evaluation: Computes key metrics (accuracy, precision, recall, F1-score, AUC-ROC) and generates confusion matrices, classification reports, and ROC curves for result analysis.

- Interpretability Tools: Integrates Grad-CAM and LIME to visualize model attention regions, helping pathologists validate model decisions.

- Easy Deployment: Supports model export to ONNX format and provides a lightweight inference script for clinical application.

### Dataset

Dataset Description

The project uses a combined dataset of bone marrow histopathology images from public sources and clinical collaborations, including:

- Public Dataset: Kaggle Bone Marrow Classification Dataset, TCIA Hematological Malignancy Collection

- Clinical Dataset: De-identified images from [Collaborating Hospital/Institution] (compliant with HIPAA and IRB regulations)

Total Samples: \~10,000 images (8,000 for training, 1,000 for validation, 1,000 for testing)

Image Specifications: 2048×2048 pixels, H&E stained, 3-channel RGB

Classification Categories: 8 types (Normal, Acute Myeloid Leukemia (AML), Myelodysplastic Syndromes (MDS), Chronic Myeloid Leukemia (CML), Multiple Myeloma (MM), Lymphoblastic Leukemia, Aplastic Anemia, Myelofibrosis)

### Installation

Prerequisites

- Python 3.8, 3.9, or 3.10

- PyTorch 1.10+ (with CUDA support recommended)

- CUDA 11.3+ (for GPU acceleration)

### Clone the Repository

git clone [https://github.com/[JhuangLab]/BMHICML.git](https://github.com/%5BJhuangLab%5D/BMHICML.git)

cd BMHICML

### Install Dependencies

Install via pip:

pip install -r requirements.txt

requirements.txt includes:

torch==1.13.1 torchvision==0.14.1 numpy==1.24.3 pandas==2.0.2 matplotlib==3.7.1

scikit-learn==1.2.2 opencv-python==4.7.0.72 pillow==9.5.0 tqdm==4.65.0

lime==0.2.0.1 grad-cam==1.4.6 onnx==1.14.0 onnxruntime==1.15.1 seaborn==0.12.2

### Quick Start

1.  Data Preparation

Organize your dataset into the data/ directory as per the Dataset Structure section.

2.  Model Training

Run the training script with default parameters (uses EfficientNet-B4 as the base model):

python train.py --config configs/efficientnet_b4.yaml

Key training parameters (modify via config file or command line):

--model: Model name (e.g., resnet50, vit_base_patch16_224) --epochs: Number of training epochs (default: 50) --batch_size: Batch size (default: 16) --lr: Initial learning rate (default: 1e-4) --data_dir: Path to dataset (default: ./data) --save_dir: Path to save models and logs (default: ./output)

3.  Model Evaluation

Evaluate a trained model on the test set:

python evaluate.py --model_path ./output/best_model.pth --test_dir ./data/test

The evaluation will generate a results/ directory containing:

- Classification report (precision, recall, F1-score)

- Confusion matrix plot

- ROC curves for each category

- Evaluation metrics CSV file

4.  Inference with Pretrained Models

Use the pretrained model for single image inference:

python infer.py --model_path ./pretrained/efficientnet_b4_best.pth --image_path ./examples/aml_sample.jpg

Sample output:

Image Path: ./examples/aml_sample.jpg Predicted Category: Acute Myeloid Leukemia (AML) Confidence Score: 0.986 Interpretability Map Saved to: ./output/grad_cam_aml_sample.png

### Interpretability

To generate interpretability maps (Grad-CAM) for model predictions:

python visualize.py --model_path ./output/best_model.pth --image_path ./examples/mds_sample.jpg --method grad-cam

The output will be a heatmap overlay on the original image, highlighting the regions that the model used to make its classification decision.

### Contribution Guidelines

We welcome contributions to improve BMHICML! Please follow these steps:

1.  Fork the repository.

2.  Create a feature branch (git checkout -b feature/your-feature).

3.  Commit your changes (git commit -m 'Add some feature').

4.  Push to the branch (git push origin feature/your-feature).

5.  Open a Pull Request.

Please ensure your code adheres to the project's coding style and includes appropriate tests.

### License

This project is licensed under the MIT License - see the LICENSE file for details.

### Contact & Citation

Contact

For questions, issues, or collaboration requests, please contact:

- Project Maintainer: [JhuangLab]

- GitHub Issues: [https://github.com/JhuangLab/BMHICML/issues](https://github.com/%5BJhuangLab%5D/BMHICML/issues)

Citation

If you use this project in your research, please cite it as:

@misc{BMHICML2024, author = {[JhuangLab], [Co-Authors]}, title = {BMHICML: Bone Marrow Histopathology Image Classification via Machine Learning}, year = {2024}, publisher = {GitHub}, journal = {GitHub Repository}, howpublished = {\url{https://github.com/[JhuangLab]/BMHICML}}, }

### Acknowledgements

- We thank our collaborator for providing clinical data.

- We acknowledge the open-source community for the foundation models and tools used in this project.
