import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IMedicalRecordController from '../../controllers/IControllers/IMedicalRecordController';

import config from "../../../config";
import { MedicalRecord } from '../../domain/medicalRecord';
import IMedicalRecordRepo from '../../services/IRepos/IMedicalRecordRepo';

const route = Router();

export default (app: Router) => {
    app.use('/medicalRecord', route);

    const ctrl = Container.get(config.controllers.medicalRecord.name) as IMedicalRecordController;
    const medicalRecordRepo = Container.get(config.repos.medicalRecord.name) as IMedicalRecordRepo;

    /*
        patientId: string;
        allergies: Allergy[];
        medicalConditions: MedicalCondition[];
    */

    route.post('/create',
        celebrate({
            body: Joi.object({
                patientId: Joi.string().guid({ version: 'uuidv4' }).required(),
                allergies: Joi.array().items(Joi.string()),
                medicalConditions: Joi.array().items(Joi.string())
            })
        }),
        (req, res, next) => ctrl.createMedicalRecord(req, res, next));


    route.get('/getAll',
        (req, res, next) => ctrl.getAllMedicalRecord(req, res, next));

    // Route to drop all medical records
    route.delete('/dropAll', async (req, res) => {
        try {
            await medicalRecordRepo.removeAll();
            res.status(200).send('All medical records have been deleted.');
        } catch (error) {
            res.status(500).send('Error deleting medical records.');
        }
    });

    route.put('/update/:id',
        celebrate({
            body: Joi.object({
                //patientId: Joi.string().guid({ version: 'uuidv4' }).required(),
                allergies: Joi.array().items(Joi.string()),
                medicalConditions: Joi.array().items(Joi.string())
            }),
            params: Joi.object({
                id: Joi.string().guid({ version: 'uuidv4' }).required()
            })
        }),
        (req, res, next) => ctrl.updateMedicalRecord(req, res, next));

    route.get('/:patientId/search',
        celebrate({
            params: Joi.object({
                patientId: Joi.string().guid({ version: 'uuidv4' }).required(),
            }),
            query: Joi.object({
                name: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.searchMedicalRecordEntries(req, res, next));
}
