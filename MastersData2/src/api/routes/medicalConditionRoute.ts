import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IMedicalConditionController from '../../controllers/IControllers/IMedicalConditionController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
    app.use('/medicalConditions', route);

    const ctrl = Container.get(config.controllers.medicalCondition.name) as IMedicalConditionController;

    route.post('/create',
        celebrate({
            body: Joi.object({
                code: Joi.string().required(),
                designation: Joi.string(),
                description: Joi.string().optional(),
                commonSymptoms: Joi.array().items(Joi.string()).optional()
            })
        }),
        (req, res, next) => ctrl.createMedicalCondition(req, res, next));

    route.get('/getAll',
        (req, res, next) => ctrl.getAllMedicalConditions(req, res, next));

    route.get('/get/:code',
        celebrate({
            params: Joi.object({
                code: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.getMedicalCondition(req, res, next));

    route.patch('/update/:code',
        celebrate({
            body: Joi.object({
                designation: Joi.string().optional(),
                description: Joi.string().optional()
            }),
            params: Joi.object({
                code: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.updateMedicalCondition(req, res, next));

    route.delete('/delete/:code',
        celebrate({
            params: Joi.object({
                code: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.deleteMedicalCondition(req, res, next));
}

