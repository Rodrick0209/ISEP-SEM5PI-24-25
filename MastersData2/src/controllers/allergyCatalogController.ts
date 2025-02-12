import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IAllergyCatalogController from './IControllers/IAllergyCatalogController';
import IAllergyCatalogService from '../services/IServices/IAllergyCatalogService';
import IAllergyCathalogItemDTO from '../dto/IAllergyCatalogItemDTO';

import { Result } from "../core/logic/Result";
import { BaseController } from '../core/infra/BaseController';
import { MongoServerError } from 'mongodb';


@Service()
export default class AllergyCatalogController extends BaseController implements IAllergyCatalogController {
    constructor(
        @Inject(config.services.allergyCatalog.name) private allergyServiceInstance : IAllergyCatalogService
    ){
        super();
    }

    public async getAllergyCatalogItem(req: Request, res: Response, next: NextFunction) {
        try {
            const allergyOrError = await this.allergyServiceInstance.getAllergyCatalogItem(req.params.code) as Result<IAllergyCathalogItemDTO>;

            if (allergyOrError.isFailure) {
                return res.status(404).send();
            }

            const allergyDTO = allergyOrError.getValue();
            return res.json( allergyDTO ).status(200);
        } catch (e) {
            return next(e);
        }
    }

    public async updateAllergyCatalogItem(req: Request, res: Response, next: NextFunction) {
        try {
            const allergyOrError = await this.allergyServiceInstance.updateAllergyCatalogItem(req.params.code, req.body as IAllergyCathalogItemDTO) as Result<IAllergyCathalogItemDTO>;

            if (allergyOrError.isFailure) {
                return res.status(400).send();
            }

            const allergyDTO = allergyOrError.getValue();
            return res.json( allergyDTO ).status(200);
        }
        catch (e) {
            return next(e);
        }
    }

    public async createAllergyCatalogItem(req: Request, res: Response, next: NextFunction) {
        try {
            const allergyOrError = await this.allergyServiceInstance.createAllergyCatalogItem(req.body as IAllergyCathalogItemDTO) as Result<IAllergyCathalogItemDTO>;
            
            if (allergyOrError.isFailure) {
                return res.status(400).send();
            }

            const allergyDTO = allergyOrError.getValue();
            return res.json( allergyDTO ).status(201);
        }
        catch (e) {
            if (e instanceof MongoServerError && e.code === 11000) {
                console.log('Duplicate key error:', e); // Log para verificar erro de chave duplicada
                return res.status(402).json({ message: 'Allergy already exists' });
              }
              console.log('Error in createAllergy:', e); // Log para verificar outros erros
              return next(e);
        }
    };


    public async getAllAllergiesItemCatalog(req: Request, res: Response, next: NextFunction) {
        try {
            const allergiesOrError = await this.allergyServiceInstance.listAllergiesCatalogItems() as Result<IAllergyCathalogItemDTO[]>;
            
            if (allergiesOrError.isFailure) {
                return res.status(404).send();
            }

            const allergiesDTO = allergiesOrError.getValue();
            return res.status(200).json( allergiesDTO );
        }
        catch (e) {
            return next(e);
        }
    };

    public async deleteAllergyCatalogItem(req: Request, res: Response, next: NextFunction) {
        try {
            const result = await this.allergyServiceInstance.deleteAllergyCatalogItem(req.params.code) as Result<void>;

            if (result.isFailure) {
                return res.status(404).send();
            }

            return res.status(204).send();
        }
        catch (e) {
            return next(e);
        }
    };


    protected async executeImpl(): Promise<void> {
        const req = this.req;
        const res = this.res;
    
        if (req.method === 'POST') {
          await this.createAllergyCatalogItem(req, res, () => {});
        } else if (req.method === 'GET') {
          await this.getAllAllergiesItemCatalog(req, res, () => {});
        } else {
          res.status(405).send('Method Not Allowed');
        }
    }

    
    





    
}