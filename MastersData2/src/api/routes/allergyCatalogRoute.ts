import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';

import IAllergyCatalogController from '../../controllers/IControllers/IAllergyCatalogController';
import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/allergiesCatalog', route);

  const ctrl = Container.get(config.controllers.allergyCatalog.name) as IAllergyCatalogController;

  route.post('/create',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        designation: Joi.string(),
        description: Joi.string().optional()
      })
    }),
    (req, res, next) => ctrl.createAllergyCatalogItem(req, res, next) );

  route.get('/getAll',
    (req, res, next) => ctrl.getAllAllergiesItemCatalog(req, res, next));

  route.put('/update/:code',
    celebrate({
      body: Joi.object({
        designation: Joi.string().optional(),
        description: Joi.string().optional()
      }),
      params: Joi.object({
        code: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.updateAllergyCatalogItem(req, res, next) );

  route.get('/get/:code',
    celebrate({
      params: Joi.object({
        code: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.getAllergyCatalogItem(req, res, next) );
}