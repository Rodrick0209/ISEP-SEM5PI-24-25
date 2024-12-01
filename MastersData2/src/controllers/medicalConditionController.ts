import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import IMedicalConditionService from '../services/IServices/IMedicalConditionService';
import IMedicalConditionDTO from '../dto/IMedicalConditionDTO';

import { Result } from "../core/logic/Result";


@Service()
export default class MedicalConditionController implements IMedicalConditionController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
      @Inject(config.services.medicalCondition.name) private medicalConditionServiceInstance : IMedicalConditionService
  ) {}

  public async createMedicalCondition(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalConditionOrError = await this.medicalConditionServiceInstance.createMedicalCondition(req.body as IMedicalConditionDTO) as Result<IMedicalConditionDTO>;
        
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

  public async getAllMedicalConditions(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalConditionsOrError = await this.medicalConditionServiceInstance.listMedicalConditions() as Result<IMedicalConditionDTO[]>;

      if (medicalConditionsOrError.isFailure) {
        return res.status(404).send();
      }

      const medicalConditionsDTO = medicalConditionsOrError.getValue();
      return res.status(201).json( medicalConditionsDTO );
    }
    catch (e) {
      return next(e);
    }
  };
}