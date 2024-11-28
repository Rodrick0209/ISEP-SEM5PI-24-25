import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';

import IAllergyController from '../../controllers/IControllers/IAllergyController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/allergies', route);

  const ctrl = Container.get(config.controllers.allergy.name) as IAllergyController;

  route.post('/create',
    celebrate({
      body: Joi.object({
        name: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createAllergy(req, res, next) );



  route.get('/getAll',
    (req, res, next) => ctrl.getAllAllergies(req, res, next));
}