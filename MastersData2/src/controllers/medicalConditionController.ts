import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IMedicalConditionController from "./IControllers/IMedicalConditionController";
import IMedicalConditionService from '../services/IServices/IMedicalConditionService';
import IMedicalConditionDTO from '../dto/IMedicalConditionCatalogDTO';

import { Result } from "../core/logic/Result";
import IMedicalConditionCatalogDTO from '../dto/IMedicalConditionCatalogDTO';


@Service()
export default class MedicalConditionController implements IMedicalConditionController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
      @Inject(config.services.medicalCondition.name) private medicalConditionServiceInstance : IMedicalConditionService
  ) {}

  public async getMedicalCondition(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalConditionOrError = await this.medicalConditionServiceInstance.getMedicalCondition(req.params.code) as Result<IMedicalConditionCatalogDTO>;

      if (medicalConditionOrError.isFailure) {
        return res.status(404).send();
      }

      const medicalConditionDTO = medicalConditionOrError.getValue();
      return res.json( medicalConditionDTO ).status(200);
    }
    catch (e) {
      return next(e);
    }
  }

  public async updateMedicalCondition(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalConditionOrError = await this.medicalConditionServiceInstance.updateMedicalCondition(req.params.code, req.body as IMedicalConditionCatalogDTO) as Result<IMedicalConditionCatalogDTO>;

      if (medicalConditionOrError.isFailure) {
        return res.status(400).send();
      }

      const medicalConditionDTO = medicalConditionOrError.getValue();
      return res.json( medicalConditionDTO ).status(200);
    } catch (e) {
      return next(e);
    }
  }

  public async createMedicalCondition(req: Request, res: Response, next: NextFunction) {
    try {
      const medicalConditionOrError = await this.medicalConditionServiceInstance.createMedicalCondition(req.body as IMedicalConditionCatalogDTO) as Result<IMedicalConditionCatalogDTO>;
        
      if (medicalConditionOrError.isFailure) {
        return res.status(400).send();
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
      const medicalConditionsOrError = await this.medicalConditionServiceInstance.listMedicalConditions() as Result<IMedicalConditionCatalogDTO[]>;

      if (medicalConditionsOrError.isFailure) {
        return res.status(404).send();
      }

      const medicalConditionsDTO = medicalConditionsOrError.getValue();
      return res.status(200).json( medicalConditionsDTO );
    }
    catch (e) {
      return next(e);
    }
  };

  public async deleteMedicalCondition(req: Request, res: Response, next: NextFunction) {
    try {
      const result = await this.medicalConditionServiceInstance.deleteMedicalCondition(req.params.code) as Result<void>;

      if (result.isFailure) {
        return res.status(404).send();
      }

      return res.status(204).send();
    } catch (e) {
      return next(e);
    }
  }
}