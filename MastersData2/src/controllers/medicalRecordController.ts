import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";
import { Result } from "../core/logic/Result";
import IMedicalRecordController from './IControllers/IMedicalRecordController';
import IMedicalRecordService from '../services/IServices/IMedicalRecordService';
import IMedicalRecordDTO from '../dto/IMedicalRecordDTO';


@Service()
export default class MedicalRecordController implements IMedicalRecordController{
  constructor(
      @Inject(config.services.medicalRecord.name) private medicalRecordServiceInstance : IMedicalRecordService
  ) {}

  public async createMedicalRecord(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalConditionOrError = await this.medicalRecordServiceInstance.createMedicalRecord(req.body as IMedicalRecordDTO) as Result<IMedicalRecordDTO>;
        
      if (medicalConditionOrError.isFailure) {
        return res.status(402).send();
      }

      const medicalConditionDTO = medicalConditionOrError.getValue();
      return res.json( medicalConditionDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };
  

  public async getAllMedicalRecord(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalRecordOrError = await this.medicalRecordServiceInstance.listMedicalRecords() as Result<IMedicalRecordDTO[]>;

      if (medicalRecordOrError.isFailure) {
        return res.status(200).send(medicalRecordOrError.error);
      }

      const medicalRecordDTO = medicalRecordOrError.getValue();
      return res.status(200).json( medicalRecordDTO );
    }
    catch (e) {
      return next(e);
    }
  };
}