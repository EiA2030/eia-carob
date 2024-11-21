# Excellence in Agronomy - Carob

This contains the *carob* compliant scripts to standardize Excellence in Agronomy (EiA) CGIAR initiative data. This is a separate repository in order to avoid risks of conflicts between the 2 repositories. For now, the datasets are not public, but once they are, they will be added to the main carob project.

For this activity, we are using EiA specific URIs. We rely on DELIVER use case descriptions to categorize the different use cases and their activities. This information can be obtained from [here](https://my.eia.cgiar.org/api/v1/usecases/). Additionally, certain adjustments need to be carried in the [`terminag`](https://github.com/reagro/terminag), such as include a new group (`eia`) and metadata variables and values. These are included on the [`terms`](https://github.com/EiA2030/eia-carob) folder of this repository. You will need to add these to the relevant installation of the `carobiner` terms.

For additional information on what is carob and how it is used, please visit [https://github.com/reagro/carob](https://github.com/reagro/carob) or the [carob-data website](https://carob-data.org/).

## Catalog of EiA datasets
|dataset_id | usecase_name | usecase_code | activity | folder_name |
|---|---|---|---|---|
| [yArGGuusw8yaoz7adsDPzjmX](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/yArGGuusw8yaoz7adsDPzjmX.R) | SEA-DSRC-Cambodia | USC012 | validation | Cambodia-DSRC-Validation |
| [fO7jxjzCPMvFPZoK6lna6H1k](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/fO7jxjzCPMvFPZoK6lna6H1k.R) | CH-CerLeg-Solidaridad | USC016 | addon | Chinyanja-Solidaridad-Soy-AddOn |
| [rMygxYbTj3FL8XryV2K96XjF](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/rMygxYbTj3FL8XryV2K96XjF.R) | CH-CerLeg-Solidaridad | USC016 | experiment | Chinyanja-Solidaridad-Soy-NOT |
| NA | CA-HighMix-OLAM | USC015 | addon | DRC-Coffee-AddOn |
| [Oxq3SaAs58lacFJ8kvx2VmmB](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/Oxq3SaAs58lacFJ8kvx2VmmB.R) | ET-HighMix-NextGen | USC006 | addon | Ethiopia-DigitalGreen-AddOn |
| [IzEfpts6gQgAGzZ4nNWRirn3](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/IzEfpts6gQgAGzZ4nNWRirn3.R) | ET-HighMix-NextGen | USC006 | validation | Ethiopia-DigitalGreen-Validation |
| [dE9ZB3cP1aKPWR9Idw4Gp5cN](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/dE9ZB3cP1aKPWR9Idw4Gp5cN.R) | ET-HighMix-Gvt ETH | USC007 | addon | Ethiopia-Fertilizer-Addon |
| [1DYRN4xC3vGwCpZTgCyCRNay](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/1DYRN4xC3vGwCpZTgCyCRNay.R) | ET-HighMix-Gvt ETH | USC007 | validation | Ethiopia-Fertilizer-Validation |
| [R8EQa4bVPWLKYBuG7m9Bjsh0](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/R8EQa4bVPWLKYBuG7m9Bjsh0.R) | GH-CerLeg-GAIP | USC009 | addon | Ghana-GAIP-AddOn |
| [doi_10.7910_DVN_F3VTAL](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/doi_10.7910_DVN_F3VTAL.R) | GH-CerLeg-GAIP | USC009 | other | NA |
| [inzOQVrqT0rowUbaxaDsJFV2](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/inzOQVrqT0rowUbaxaDsJFV2.R) | GH-CerLeg-Esoko | USC010 | experiment | Ghana-Soybean-NOT |
| [ttkFAIbCvRiUzQIIZMM1z0Yj](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/ttkFAIbCvRiUzQIIZMM1z0Yj.R) | WA-Rice-ATAFI/MOVE | USC001 | addon | Nigeria-ATAFI-AddOn |
| [NT7VLWDrOQEkarWkfn2bDSuf](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/NT7VLWDrOQEkarWkfn2bDSuf.R) | WA-Rice-ATAFI/MOVE | USC001 | validation | Nigeria-ATAFI-AddOn |
| NA | NG-Akilimo-SAA | USC008 | addon | Nigeria-SAA-AdOn |
| NA | NG-Akilimo-SAA | USC008 | experiment | Nigeria-SAA-Experiment-PlantingDate |
| [ZhlNGvfIy9DNUdaDXvQkTBMC](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/ZhlNGvfIy9DNUdaDXvQkTBMC.R) | NG-Akilimo-SAA | USC008 | validation | Nigeria-SAA-Validation |
| [7U1xobHbcJa9sHwXMlqmt6xy](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/7U1xobHbcJa9sHwXMlqmt6xy.R) | NG-Akilimo-MC Sprout | USC013 | addon | Nigeria-Sprout-AddOn |
| [YkRtc9XRGbxsZKbpN7SMbHc7](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/YkRtc9XRGbxsZKbpN7SMbHc7.R) | CA-HighMix-SNS/RAB | USC002 | addon | Rwanda-RAB-AddOn |
| [S7C2vKKGuDfMbQmayY5BrAfC](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/S7C2vKKGuDfMbQmayY5BrAfC.R) | CA-HighMix-SNS/RAB | USC002 | other | Rwanda-RAB-Rice-partners |
| [m1Q1vJBBuBPWhNQ3Zh2GITd1](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/m1Q1vJBBuBPWhNQ3Zh2GITd1.R) | CA-HighMix-SNS/RAB | USC002 | validation | Rwanda-RAB-Validation |
| [ZhlNGvfIy9DNUdaDXvQkTBMC](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/ZhlNGvfIy9DNUdaDXvQkTBMC.R) | NG-Akilimo-SAA | USC008 | validation | SA-PlantingDate-Validation |
| [hdl_11529_10549106](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/hdl_11529_10549106.R) | LatAm-AgroTutor | USC005 | validation | NA |
| [1KYnKWUTZbXbnLnb9dB2Wjci](https://github.com/EiA2030/eia-carob/tree/main/scripts/eia/1KYnKWUTZbXbnLnb9dB2Wjci.R) | CH-CerLeg-Solidaridad | USC016 | other | Chinyanja-Solidaridad-Soy-CropCut |
