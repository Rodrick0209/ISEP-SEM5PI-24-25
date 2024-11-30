import { Request, Response, NextFunction } from 'express';

export default interface IAllergyController  {
    createAllergyCatalogItem(req: Request, res: Response, next: NextFunction);
    getAllAllergies(req: Request, res: Response, next: NextFunction);

}