import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';

import IAllergyCatalogController from '../../controllers/IControllers/IAllergyController';
import isAuth from '../middlewares/isAuth';
import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/allergies', route);

  const ctrl = Container.get(config.controllers.allergyCatalog.name) as IAllergyCatalogController;

  route.post('/create', isAuth,
    celebrate({
      body: Joi.object({
        name: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createAllergyCatalogItem(req, res, next) );



  route.get('/getAll',
    (req, res, next) => ctrl.getAllAllergies(req, res, next));
}