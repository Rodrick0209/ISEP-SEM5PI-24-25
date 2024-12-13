import { Request, Response, NextFunction } from 'express';

export default interface IAllergyCatalogController  {
    createAllergyCatalogItem(req: Request, res: Response, next: NextFunction);
    getAllAllergiesItemCatalog(req: Request, res: Response, next: NextFunction);
    updateAllergyCatalogItem(req: Request, res: Response, next: NextFunction);
    getAllergyCatalogItem(req: Request, res: Response, next: NextFunction);
}