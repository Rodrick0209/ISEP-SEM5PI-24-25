import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IAllergyController from './IControllers/IAllergyController';
import IAllergyService from '../services/IServices/IAllergyService';
import IAllergyDTO from '../dto/IAllergyDTO';

import { Result } from "../core/logic/Result";
import { BaseController } from '../core/infra/BaseController';


@Service()
export default class AllergyController extends BaseController implements IAllergyController {
    constructor(
        @Inject(config.services.allergy.name) private allergyServiceInstance : IAllergyService
    ){
        super();
    }

    public async createAllergy(req: Request, res: Response, next: NextFunction) {
        try {
            const allergyOrError = await this.allergyServiceInstance.createAllergy(req.body as IAllergyDTO) as Result<IAllergyDTO>;
            
            if (allergyOrError.isFailure) {
                console.log('Allergy creation failed'); // Log para verificar falha na criação
                return res.status(402).send();
            }

            const allergyDTO = allergyOrError.getValue();
            return res.json( allergyDTO ).status(201);
        }
        catch (e) {
            console.log('Error in createAllergy:', e); // Log para verificar erros
            return next(e);
        }
    };


    public async getAllAllergies(req: Request, res: Response, next: NextFunction) {
        try {
            const allergiesOrError = await this.allergyServiceInstance.listAllergies() as Result<IAllergyDTO[]>;
            
            if (allergiesOrError.isFailure) {
                return res.status(404).send();
            }

            const allergiesDTO = allergiesOrError.getValue();
            return res.status(201).json( allergiesDTO );
        }
        catch (e) {
            return next(e);
        }
    };

    protected async executeImpl(): Promise<void> {
        const req = this.req;
        const res = this.res;
    
        if (req.method === 'POST') {
          await this.createAllergy(req, res, () => {});
        } else if (req.method === 'GET') {
          await this.getAllAllergies(req, res, () => {});
        } else {
          res.status(405).send('Method Not Allowed');
        }
    }
    





    
}