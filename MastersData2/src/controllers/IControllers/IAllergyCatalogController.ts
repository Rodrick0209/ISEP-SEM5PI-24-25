import { Request, Response, NextFunction } from 'express';

export default interface IAllergyCatalogController  {
    createAllergyCatalogItem(req: Request, res: Response, next: NextFunction);
    getAllAllergiesItemCatalog(req: Request, res: Response, next: NextFunction);

}