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
                name: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.createMedicalCondition(req, res, next));

    route.get('/getAll',
        (req, res, next) => ctrl.getAllMedicalConditions(req, res, next));

    route.get('/get/:name',
        celebrate({
            params: Joi.object({
                name: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.getMedicalCondition(req, res, next));

    route.put('/update/:name',
        celebrate({
            body: Joi.object({
                nameToEdit: Joi.string().required()
            }),
            params: Joi.object({
                name: Joi.string().required()
            })
        }),
        (req, res, next) => ctrl.updateMedicalCondition(req, res, next));
}

