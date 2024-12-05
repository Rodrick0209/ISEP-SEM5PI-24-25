import { Request, Response, NextFunction } from 'express';

export default interface IMedicalRecordController  {
    createMedicalRecord(req: Request, res: Response, next: NextFunction);


    getAllMedicalRecord(req: Request, res: Response, next: NextFunction);

    updateMedicalRecord(req: Request, res: Response, next: NextFunction);

}